-- | One authority for authenticated previews and transferable document grants.
module HMem.Server.Document.Grant where

import Control.Exception (Exception,fromException,throwIO)
import Control.Monad (unless,when)
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding (convertToBase,Base(Base64URLUnpadded))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAscii,isAlphaNum,isSpace,isHexDigit,toLower)
import Data.IORef
import Data.Maybe (isJust)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime,addUTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUIDText
import Data.UUID.V4 qualified as UUID
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hasql.Connection qualified as H
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAllow,hContentDisposition,hContentLength,hETag,hHost)
import Network.Socket (SockAddr(..),hostAddressToTuple)
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Servant (ServerError(..),runHandler)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Numeric (showHex)
import HMem.Auth.CredentialOrigin
import HMem.Config qualified as C
import HMem.DB.Auth qualified as Auth
import HMem.DB.DocumentGrant qualified as DB
import HMem.DB.RequestContext
import HMem.Document.Codec (chunks,payloadSha256)
import HMem.Document.Grant
import HMem.Document.Types (DocumentException(..))
import HMem.Server.API (authorizeTaskEvidenceRead)
import HMem.Server.Document.HTTP (safeHeaders,originalTransferHeaders,downloadAssociationQuery,authorizeDocumentDownload)
import HMem.Server.Exception (trySynchronous)

data Origin=Origin { value :: !Text, hostname :: !Text, secure :: !Bool, port :: !Int } deriving stock(Show,Eq)
data AuthorityPolicy=AuthorityPolicy
  { auth :: !C.AuthConfig, server :: !C.ServerConfig, delivery :: !C.DocumentDeliveryConfig
  , epoch :: !UUID, applicationOrigin :: !(Maybe Origin), downloadOrigin :: !(Maybe Origin), previewOrigin :: !(Maybe Origin)
  } deriving stock(Show,Eq)
data DocumentAuthority=DocumentAuthority { pool :: !(Pool H.Connection), policy :: !(IORef AuthorityPolicy) }

newDocumentAuthority :: Pool H.Connection -> C.AuthConfig -> C.ServerConfig -> C.DocumentDeliveryConfig -> IO DocumentAuthority
newDocumentAuthority pool auth server delivery=DocumentAuthority pool <$> (makePolicy auth server delivery >>=newIORef)

-- Relevant configuration changes invalidate all prior delegation; startup also
-- chooses a fresh epoch. Ordinary Principal/effective-actor semantics stay intact.
replaceDocumentAuthorityPolicy :: DocumentAuthority -> C.AuthConfig -> C.ServerConfig -> C.DocumentDeliveryConfig -> IO()
replaceDocumentAuthorityPolicy authority auth server delivery=makePolicy auth server delivery >>=writeIORef authority.policy

makePolicy :: C.AuthConfig -> C.ServerConfig -> C.DocumentDeliveryConfig -> IO AuthorityPolicy
makePolicy auth server delivery=do
  epoch<-UUID.nextRandom
  app<-traverse (parseOrigin auth server) delivery.applicationOrigin
  download<-traverse (parseOrigin auth server) delivery.downloadOrigin
  preview<-traverse (parseOrigin auth server) delivery.previewOrigin
  let applicationOrigin=joinMaybe app
      downloadOrigin=if isJust applicationOrigin then joinMaybe download else Nothing
      candidate=joinMaybe preview
      previewOrigin=do
        application<-applicationOrigin
        downloadValue<-downloadOrigin
        selected<-candidate
        scopes<-delivery.proxyAuthCookieDomains
        normalized<-traverse cookieDomain scopes
        port<-delivery.previewListenPort
        if selected.hostname `elem` [application.hostname,downloadValue.hostname]
          || any (cookieCovers selected.hostname) normalized || not(C.serverHostIsLoopback delivery.previewListenHost)
          || port<1 || port>65535 then Nothing else Just selected
  pure(AuthorityPolicy auth server delivery epoch applicationOrigin downloadOrigin previewOrigin)
  where joinMaybe=maybe Nothing id

parseOrigin :: C.AuthConfig -> C.ServerConfig -> Text -> IO(Maybe Origin)
parseOrigin auth server raw
  | T.length raw>4096 || T.any (\c -> not(isAscii c)||isSpace c||c<'!'||c `elem` ['@','?','#','%','\\']) raw
      || not(any (`T.isPrefixOf` raw) ["https://","http://"]) = pure Nothing
  | otherwise=do
      parsed<-trySynchronous(HTTP.parseRequest(T.unpack raw))
      case parsed of
        Left _->pure Nothing
        Right request->do
          let host=T.toLower(T.dropAround (`elem` ['[',']'])(TE.decodeUtf8(HTTP.host request)))
              scheme=if HTTP.secure request then "https://" else "http://"
              port=HTTP.port request
              standard=(HTTP.secure request&&port==443)||(not(HTTP.secure request)&&port==80)
              renderedHost=if T.any(==':')host then "["<>host<>"]" else host
              rendered=scheme<>renderedHost<>(if standard then "" else ":"<>T.pack(show port))
              normalized=T.toLower(maybe raw id(T.stripSuffix "/" raw))
              exactPort=normalized==rendered || (standard && normalized==rendered<>":"<>T.pack(show port))
          identity<-canonicalHost host
          pure $ do
            canonical<-identity
            if HTTP.path request/="/" || not(BS.null(HTTP.queryString request)) || not(null(HTTP.requestHeaders request))
              || not exactPort || port<1 || port>65535
              || not(HTTP.secure request || (auth.mode==C.AuthModeLocal && C.serverHostIsLoopback server.host && C.serverHostIsLoopback canonical))
              then Nothing else Just(Origin rendered canonical (HTTP.secure request) port)

-- Canonical identity prevents numeric-IP aliases from bypassing cookie-host
-- comparisons. Numeric IPv6 parsing uses AI_NUMERICHOST and performs no DNS.
canonicalHost :: Text -> IO(Maybe Text)
canonicalHost host
  | T.null host || T.length host>253 || T.isPrefixOf "." host || T.isSuffixOf "." host=pure Nothing
  | T.any(==':')host=do
      parsed<-trySynchronous(Socket.getAddrInfo
        (Just Socket.defaultHints{Socket.addrFlags=[Socket.AI_NUMERICHOST],Socket.addrFamily=Socket.AF_INET6}) (Just(T.unpack host)) Nothing)
      pure $ case parsed of
        Left _->Nothing
        Right values->case [address|entry<-values,SockAddrInet6 _ _ address _<-[Socket.addrAddress entry]] of
          address:_->let(a,b,c,d,e,f,g,h)=Socket.hostAddress6ToTuple address in
            Just(if address==(0,0,0,1) then "::1" else T.intercalate ":" [hex a,hex b,hex c,hex d,hex e,hex f,hex g,hex h])
          _->Nothing
  | numericEnding= pure $ do
      numbers<-traverse decimal labels
      if length numbers==4 && T.intercalate "."(map(T.pack . show)numbers)==host then Just host else Nothing
  | otherwise=pure(cookieDomain host)
  where
    labels=T.splitOn "." host
    ending=last labels
    digit c=c>='0'&&c<='9'
    numericEnding=T.all digit ending || maybe False (\suffix->not(T.null suffix)&&T.all isHexDigit suffix)(T.stripPrefix "0x" ending)
    decimal part=if T.length part>3 || not(T.all digit part) then Nothing else do
      number<-readMaybe(T.unpack part)::Maybe Int
      if number>=0&&number<=255 then Just number else Nothing
    hex value=T.pack(showHex value "")

cookieDomain :: Text -> Maybe Text
cookieDomain original=let value=T.toLower(maybe original id(T.stripPrefix "." original)); labels=T.splitOn "." value
  in if T.null value || T.length value>253 || any (\part -> T.null part || T.length part>63 || not(T.all(\c -> isAscii c&&(isAlphaNum c||c=='-'))part)
        || T.isPrefixOf "-" part || T.isSuffixOf "-" part) labels then Nothing else Just value
cookieCovers :: Text -> Text -> Bool
cookieCovers host domain=host==domain || ("."<>domain) `T.isSuffixOf` host

previewListener :: DocumentAuthority -> IO(Maybe(Text,Int))
previewListener authority=do
  policy<-readIORef authority.policy
  pure $ do _<-policy.previewOrigin; port<-policy.delivery.previewListenPort; pure(policy.delivery.previewListenHost,port)

-- Host is only a fail-closed deployment check against trusted configuration,
-- never a source of URLs or authority. No forwarded-host fallback is accepted.
validatePreviewAuthority :: DocumentAuthority -> Wai.Request -> IO()
validatePreviewAuthority authority request=do
  policy<-readIORef authority.policy
  expected<-maybe(failGrant status404 "styled_preview_unavailable")pure policy.previewOrigin
  raw<-case [host|(header,host)<-Wai.requestHeaders request,header==hHost] of
    [host]->either(const(failGrant wrongOrigin "preview_origin_mismatch"))pure(TE.decodeUtf8' host)
    _->failGrant wrongOrigin "preview_origin_mismatch"
  when(T.any(=='/')raw)(failGrant wrongOrigin "preview_origin_mismatch")
  incoming<-parseOrigin policy.auth policy.server ((if expected.secure then "https://" else "http://")<>raw)
  unless(maybe False(\origin->origin.hostname==expected.hostname && origin.port==expected.port)incoming)
    (failGrant wrongOrigin "preview_origin_mismatch")
  where wrongOrigin=mkStatus 421 "Misdirected Request"

documentFramePolicy :: DocumentAuthority -> Wai.Middleware
documentFramePolicy authority app request respond=do
  policy<-readIORef authority.policy
  let origin=maybe "'none'" (.value) policy.previewOrigin
      csp="frame-src "<>TE.encodeUtf8 origin<>"; child-src "<>TE.encodeUtf8 origin<>"; worker-src 'self'"
  app request (respond . Wai.mapResponseHeaders (<>[("Content-Security-Policy",csp)]))

data GrantFailure=GrantFailure Status Text deriving stock(Show)
instance Exception GrantFailure
newtype AccessFailure=AccessFailure ServerError deriving stock(Show)
instance Exception AccessFailure
failGrant :: Status -> Text -> IO a
failGrant status code=throwIO(GrantFailure status code)
jsonResponse :: Status -> Value -> Wai.Response
jsonResponse status value=Wai.responseLBS status ((hContentType,"application/json"):safeHeaders)(encode value)
errorResponse :: Status -> Text -> Wai.Response
errorResponse status code=jsonResponse status(object["error" .= code,"message" .= code])
allowMethods :: ByteString -> Wai.Response -> Wai.Response
allowMethods methods response=if Wai.responseStatus response==status405
  then Wai.mapResponseHeaders ((hAllow,methods):) response else response
safeFailure :: IO Wai.Response -> IO Wai.Response
safeFailure action=trySynchronous action >>= \case
  Right response->pure response
  Left failure
    | Just(GrantFailure status code)<-fromException failure->pure(errorResponse status code)
    | Just(AccessFailure denied)<-fromException failure->pure(Wai.responseLBS(mkStatus denied.errHTTPCode(B8.pack denied.errReasonPhrase))(safeHeaders<>denied.errHeaders)denied.errBody)
    | otherwise->pure(errorResponse status503 "document_unavailable")

requestIsLoopback :: Wai.Request -> Bool
requestIsLoopback request=case Wai.remoteHost request of
  SockAddrInet _ address->let(a,_,_,_)=hostAddressToTuple address in a==127
  SockAddrInet6 _ _ address _->address==(0,0,0,1)
  _->False

principalMatches :: Principal -> Principal -> Bool
principalMatches first second=first.actorType==second.actorType && first.actorId==second.actorId && first.authority==second.authority

authorizeIssuer :: DocumentAuthority -> Wai.Request -> StoredGrant -> IO AuthorityPolicy
authorizeIssuer authority request grant=do
  policy<-readIORef authority.policy
  unless(policy.epoch==grant.authorityEpoch)(failGrant status403 "grant_revoked")
  current<-case grant.issuer.credentialOrigin of
    LocalOrigin->do
      unless(policy.auth.mode==C.AuthModeLocal && C.serverHostIsLoopback policy.server.host && requestIsLoopback request
        && grant.issuer.principal.authority==PrincipalSyntheticLocalSuperadmin)(failGrant status403 "grant_revoked")
      pure(ResolvedRequestAuth grant.issuer.principal LocalOrigin Nothing)
    origin->do
      unless(policy.auth.mode==C.AuthModeDeployed)(failGrant status403 "grant_revoked")
      Auth.revalidateCredentialOrigin authority.pool origin >>=maybe(failGrant status403 "grant_revoked")pure
  unless(sameIssuer grant.issuer current)(failGrant status403 "grant_revoked")
  allowed<-Auth.authorizeWorkspace authority.pool (Just current.principal) grant.binding.workspaceId Auth.WorkspaceRoleRead
  either (const(failGrant status403 "grant_revoked")) pure allowed
  DB.bindingIsCurrent authority.pool grant.binding >>= \valid->unless valid(failGrant status404 "grant_not_found")
  when(grant.issuer.botRestricted || grant.issuer.principal.actorType==ActorBot) $ do
    let binding=grant.binding
    _<-authorizeDocumentDownload authority.pool True binding.workspaceId binding.taskId binding.documentId
      (Just(binding.role,binding.associationId)) >>= either (uncurry failGrant) pure
    pure()
  pure policy

-- No raw bearer is stored in a runtime record or database row.
freshGrant :: DocumentAuthority -> Wai.Request -> Bool -> UUID -> Text -> Text -> Int -> IO StoredGrant
freshGrant authority request preview identifier tokenHash variant page=do
  grant<-DB.lookupGrant authority.pool identifier tokenHash >>=maybe(failGrant status404 "grant_not_found")pure
  unless(grant.binding.variant==variant && grant.binding.page==page)(failGrant status404 "grant_not_found")
  policy<-authorizeIssuer authority request grant
  if preview then do
    unless(variant=="styled_html_preview" && isJust policy.previewOrigin)(failGrant status404 "grant_not_found")
    else unless(variant/="styled_html_preview" && isJust policy.downloadOrigin)(failGrant status404 "grant_not_found")
  pure grant

tokenForRequest :: Bool -> Wai.Request -> IO (Text,Bool)
tokenForRequest allowFrame request=case Wai.queryString request of
  [("token",Just token)]->validate token False
  [("token",Just token),("view",Just "frame")] | allowFrame->validate token True
  [("view",Just "frame"),("token",Just token)] | allowFrame->validate token True
  _->failGrant status400 "invalid_grant_token"
  where
    validate token framed
      | BS.length token==43 && BS.all(\c -> (c>=65&&c<=90)||(c>=97&&c<=122)||(c>=48&&c<=57)||c==45||c==95)token =
          pure(TE.decodeUtf8 token,framed)
      | otherwise=failGrant status400 "invalid_grant_token"

grantRoute :: Wai.Request -> Maybe(UUID,Text,Int)
grantRoute request=case Wai.pathInfo request of
  ["document-grants",identifier,variant,page]->(,,) <$> UUIDText.fromText identifier <*>pure variant <*>readMaybe(T.unpack page)
  _->Nothing

documentRedemptionMiddleware :: DocumentAuthority -> Wai.Middleware -> Wai.Middleware
documentRedemptionMiddleware authority admit app request respond=case Wai.pathInfo request of
  "document-grants":_->admit (redeemApplication authority False) request (respond . Wai.mapResponseHeaders documentResponseHeaders)
  _->app request respond

documentResponseHeaders :: [Header] -> [Header]
documentResponseHeaders headers=safeHeaders<>filter(\(name,_)->not(elem name(map fst safeHeaders)))headers

-- The dedicated app calls this with preview=True and has no fallback app/router.
redeemApplication :: DocumentAuthority -> Bool -> Wai.Application
redeemApplication authority preview request respond=do
  deadline<-((+300000000000) <$>getMonotonicTimeNSec)
  safeFailure(bounded deadline(prepare deadline)) >>=respond . allowMethods "GET, HEAD"
  where
    prepare deadline=do
      when preview(validatePreviewAuthority authority request)
      when(preview && any(\(header,_)->header==hAuthorization||header==hCookie)(Wai.requestHeaders request))
        (failGrant status400 "application_credentials_forbidden")
      unless(Wai.requestMethod request `elem` [methodGet,methodHead])(failGrant status405 "method_not_allowed")
      (identifier,variant,page)<-maybe(failGrant status404 "grant_not_found")pure(grantRoute request)
      (token,framed)<-tokenForRequest (preview && variant=="styled_html_preview") request
      let tokenHash=payloadSha256(TE.encodeUtf8 token)
          check=freshGrant authority request preview identifier tokenHash variant page
      grant<-check
      policy<-readIORef authority.policy
      let binding=grant.binding
          rawHeaders=if variant=="original" then originalTransferHeaders binding.filename binding.rawBytes binding.rawSha256
            else representationHeaders binding (if preview then fmap (.value) policy.applicationOrigin else Nothing)
          headers=if framed then filter ((\name->name `notElem` [hContentLength,hETag,"X-HMem-Document-SHA256","X-HMem-Document-Bytes"]) . fst) rawHeaders else rawHeaders
      if Wai.requestMethod request==methodHead then pure(Wai.responseLBS status200 headers "") else do
        bytes<-bounded deadline(DB.readBoundPayload authority.pool binding)
        _<-check
        pure(Wai.responseStream status200 headers $ \send flush->voidBounded deadline $ do
          when framed(send styledFrameStart)
          mapM_ (\part->do _<-check; send(if framed then escapeSrcdoc part else Builder.byteString part); flush)(chunks bytes)
          when framed $ do
            -- No child frame is parsed before the entire authorized attribute arrives.
            _<-check
            send "\"></iframe>"
            flush)

styledFrameStart :: Builder.Builder
styledFrameStart="<!doctype html><meta charset=utf-8><title>Styled preview</title><body style=\"margin:0;overflow:hidden\"><iframe title=\"Styled preview\" sandbox=\"\" referrerpolicy=\"no-referrer\" style=\"display:block;width:100%;height:100vh;border:0\" srcdoc=\""

escapeSrcdoc :: ByteString -> Builder.Builder
escapeSrcdoc input=
  let (plain,rest)=BS.break (\c->c==34||c==38||c==60||c==62) input
  in Builder.byteString plain<>case BS.uncons rest of
    Nothing->mempty
    Just (c,tailBytes)->
      (case c of
        34->"&quot;"
        38->"&amp;"
        60->"&lt;"
        _->"&gt;")<>escapeSrcdoc tailBytes

representationHeaders :: GrantBinding -> Maybe Text -> [Header]
representationHeaders binding appOrigin=safeHeaders<>
  [(hContentType,if binding.variant=="styled_html_preview" then "text/html;charset=utf-8" else TE.encodeUtf8 binding.mediaType)
  ,(hContentLength,B8.pack(show binding.rawBytes)),(hContentDisposition,"inline")
  ,(hETag,"\""<>TE.encodeUtf8 binding.rawSha256<>"\"")
  ,("X-HMem-Document-SHA256",TE.encodeUtf8 binding.rawSha256),("X-HMem-Document-Bytes",B8.pack(show binding.rawBytes))
  ,("Content-Security-Policy",if binding.variant=="styled_html_preview" then
    styledPreviewCSP appOrigin
    else "default-src 'none'; sandbox")]

styledPreviewCSP :: Maybe Text -> ByteString
styledPreviewCSP appOrigin="default-src 'none'; script-src 'none'; style-src 'unsafe-inline'; img-src data:; font-src 'none'; connect-src 'none'; frame-src 'none'; child-src 'none'; worker-src 'none'; object-src 'none'; media-src 'none'; base-uri 'none'; form-action 'none'; sandbox; frame-ancestors "<>maybe "'none'" TE.encodeUtf8 appOrigin

previewResponseCSP :: DocumentAuthority -> IO ByteString
previewResponseCSP authority=styledPreviewCSP . fmap (.value) . (.applicationOrigin) <$>readIORef authority.policy

bounded :: Word64 -> IO a -> IO a
bounded deadline action=do
  now<-getMonotonicTimeNSec
  when(now>=deadline)(failGrant status504 "document_timeout")
  timeout(fromIntegral((deadline-now) `div` 1000))action >>=maybe(failGrant status504 "document_timeout")pure
voidBounded :: Word64 -> IO() -> IO()
voidBounded=bounded


data DocumentGrantRoute=CurrentPreview | RasterPreview UUID Text Int | IssueGrant | RevokeGrant UUID
documentGrantRoute :: Wai.Request -> Maybe(UUID,UUID,UUID,DocumentGrantRoute)
documentGrantRoute request=case Wai.pathInfo request of
  "api":"v1":"workspaces":workspace:"tasks":task:"documents":document:rest->do
    w<-UUIDText.fromText workspace; t<-UUIDText.fromText task; d<-UUIDText.fromText document
    route<-case rest of
      ["previews","current"]->Just CurrentPreview
      ["previews",generation,variant,page]->RasterPreview <$>UUIDText.fromText generation <*>pure variant <*>readMaybe(T.unpack page)
      ["download-grants"]->Just IssueGrant
      ["download-grants",identifier]->RevokeGrant <$>UUIDText.fromText identifier
      _->Nothing
    pure(w,t,d,route)
  _->Nothing

-- This runs inside the ordinary principal/CSRF boundary. The captured context
-- preserves effective actor attribution; authorization uses only Principal.
documentGrantMiddleware :: DocumentAuthority -> (Wai.Request->IO(Maybe ResolvedRequestAuth)) -> Wai.Middleware
documentGrantMiddleware authority resolve app request respond=case documentGrantRoute request of
  Nothing->app request respond
  Just(workspace,task,document,route)->do
    original<-currentRequestContext
    requestDeadline<-((+300000000000) <$>getMonotonicTimeNSec)
    let authorize=do
          fresh<-resolve request >>=maybe(failGrant status401 "unauthorized")pure
          unless(Just fresh.principal==original.principal)(failGrant status401 "unauthorized")
          result<-withRequestContext original (runHandler(authorizeTaskEvidenceRead authority.pool workspace task))
          either(throwIO . AccessFailure)pure result
          pure fresh
        current=DB.previewStatus authority.pool workspace task document >>=maybe(failGrant status404 "document_not_found")pure
        authorizeBytes selection=authorizeDocumentDownload authority.pool (isBotRequest original) workspace task document selection
          >>= either (uncurry failGrant) pure
        pinned generation variant page=do
          unless(elem variant ["page","thumbnail"] &&validRepresentation variant page(Just generation))
            (failGrant status400 "document_preview_representation")
          status<-current
          unless(status.state=="ready")(failGrant status409 "document_preview_unavailable")
          unless(status.generationId==Just generation)(failGrant status409 "document_preview_stale")
          DB.resolveVisibleBinding authority.pool workspace task document variant page(Just generation)
            >>=maybe(failGrant status404 "document_preview_not_found")pure
        prepare=do
          fresh<-authorize
          case (Wai.requestMethod request,route) of
            ("GET",CurrentPreview)->jsonResponse status200 . toJSON <$>current
            (method,RasterPreview generation variant page) | elem method [methodGet,methodHead]->do
              selection<-either (failGrant status400) pure (downloadAssociationQuery request)
              association<-authorizeBytes selection
              binding<-pinned generation variant page
              let check=do
                    _<-authorize
                    _<-authorizeBytes association
                    found<-pinned generation variant page
                    unless(sameRepresentation binding found)(failGrant status409 "document_preview_stale")
              let headers=representationHeaders binding Nothing
              if method==methodHead then pure(Wai.responseLBS status200 headers "") else do
                let deadline=requestDeadline
                bytes<-bounded deadline(DB.readBoundPayload authority.pool binding)
                check
                pure(Wai.responseStream status200 headers $ \send flush->voidBounded deadline $
                  mapM_(\part->check>>send(Builder.byteString part)>>flush)(chunks bytes))
            ("POST",IssueGrant)->do
              when(fresh.credentialOrigin==JwtOrigin)(failGrant status403 "credential_authority_unavailable")
              initialPolicy<-readIORef authority.policy
              when(fresh.credentialOrigin==LocalOrigin) $
                unless(initialPolicy.auth.mode==C.AuthModeLocal && C.serverHostIsLoopback initialPolicy.server.host
                  && requestIsLoopback request &&fresh.principal.authority==PrincipalSyntheticLocalSuperadmin)
                  (failGrant status403 "credential_authority_unavailable")
              _<-current
              input<-collectGrantRequest request
              latest<-authorize
              policy<-readIORef authority.policy
              origin<-maybe(failGrant status503 (if input.variant=="styled_html_preview" then "styled_preview_unavailable" else "document_download_unavailable"))pure
                (if input.variant=="styled_html_preview" then policy.previewOrigin else policy.downloadOrigin)
              when(latest.credentialOrigin==LocalOrigin) $
                unless(policy.auth.mode==C.AuthModeLocal && C.serverHostIsLoopback policy.server.host && requestIsLoopback request
                  && C.serverHostIsLoopback origin.hostname &&latest.principal.authority==PrincipalSyntheticLocalSuperadmin)
                  (failGrant status403 "credential_authority_unavailable")
              _<-authorizeBytes (Just(input.role,input.associationId))
              binding<-DB.resolveBinding authority.pool workspace task document input >>=maybe(failGrant status404 "document_representation_not_found")pure
              now<-getCurrentTime
              let requested=addUTCTime(fromIntegral(maybe 300 id input.ttlSeconds))now
                  expiry=maybe requested(min requested)latest.credentialExpiresAt
              unless(expiry>now)(failGrant status401 "credential_expired")
              bytes<-getRandomBytes 32::IO ByteString
              let token=TE.decodeUtf8(convertToBase Base64URLUnpadded bytes)
                  tokenHash=payloadSha256(TE.encodeUtf8 token)
                  issuer=GrantIssuer latest.principal latest.credentialOrigin (isBotRequest original)
              identifier<-UUID.nextRandom
              stored<-DB.createGrant authority.pool identifier tokenHash issuer policy.epoch binding expiry
              _<-freshGrant authority request (input.variant=="styled_html_preview")identifier tokenHash input.variant input.page
              let url=origin.value<>"/document-grants/"<>UUIDText.toText identifier<>"/"<>input.variant<>"/"<>T.pack(show input.page)<>"?token="<>token
                  response=case publicBinding binding of
                    Object fields->Object(KM.insert "grant_id"(toJSON identifier)(KM.insert "url"(toJSON url)(KM.insert "expires_at"(toJSON stored.expiresAt)fields)))
                    _->Null
              pure(jsonResponse status201 response)
            ("DELETE",RevokeGrant identifier)->do
              found<-DB.getGrant authority.pool identifier >>=maybe(failGrant status404 "grant_not_found")pure
              unless(found.binding.workspaceId==workspace &&found.binding.taskId==task &&found.binding.documentId==document)
                (failGrant status404 "grant_not_found")
              admin<-Auth.hasWorkspaceRole authority.pool (Just fresh.principal) workspace Auth.WorkspaceRoleAdmin
              unless(admin||principalMatches found.issuer.principal fresh.principal)(failGrant status403 "forbidden")
              _<-DB.revokeGrant authority.pool workspace task document identifier
              pure(Wai.responseLBS status204 safeHeaders "")
            _->failGrant status405 "method_not_allowed"
    let methods=case route of CurrentPreview->"GET"; RasterPreview _ _ _->"GET, HEAD"; IssueGrant->"POST"; RevokeGrant _->"DELETE"
    withWorkspaceIdContext(Just workspace)(safeFailure(bounded requestDeadline prepare) >>=respond . allowMethods methods)

sameRepresentation :: GrantBinding -> GrantBinding -> Bool
sameRepresentation first second=(first{role=second.role,associationId=second.associationId}::GrantBinding)==second

collectGrantRequest :: Wai.Request -> IO GrantRequest
collectGrantRequest request=do
  case [value|(header,value)<-Wai.requestHeaders request,header==hContentType] of
    [value] | B8.map toLower(B8.takeWhile(/=';')value)=="application/json"->pure()
    _->failGrant status415 "json_required"
  expected<-case [value|(header,value)<-Wai.requestHeaders request,header==hContentLength] of
    []->pure Nothing
    [value]->case readMaybe(B8.unpack value)::Maybe Integer of
      Just size | size>=0 &&size<=4096->pure(Just size)
      _->failGrant status413 "grant_body_limit"
    _->failGrant status400 "invalid_content_length"
  case Wai.requestBodyLength request of
    Wai.KnownLength size | size>4096->failGrant status413 "grant_body_limit"
    Wai.KnownLength size | maybe False(/=fromIntegral size)expected->failGrant status400 "invalid_content_length"
    _->pure()
  let collect count parts=do
        chunk<-Wai.getRequestBodyChunk request
        when(BS.length chunk>4096-count)(failGrant status413 "grant_body_limit")
        let next=count+BS.length chunk
        if BS.null chunk then do
          when(maybe False(/=fromIntegral count)expected)(failGrant status400 "invalid_content_length")
          case Wai.requestBodyLength request of
            Wai.KnownLength size | fromIntegral count/=size->failGrant status400 "invalid_content_length"
            _->pure()
          pure(LBS.fromChunks(reverse parts))
        else collect next(chunk:parts)
  bytes<-collect 0 []
  either(const(failGrant status400 "invalid_grant_request"))pure(eitherDecode bytes)

