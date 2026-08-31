-- | Redaction-reviewed snapshot materialization for the public change-stream.
-- The SQL below is intentionally an explicit allowlist rather than @to_jsonb@
-- over tables: adding a column cannot accidentally make it transport-visible.
module HMem.Server.Snapshot
  ( materializeSnapshot
  , materializeSnapshotWithProfile
  ) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Session qualified as Session
import Hasql.Statement qualified as Statement

import HMem.DB.ChangeStream (ChangeScope(..))
import HMem.Types (SnapshotProfile(..))

materializeSnapshot :: ChangeScope -> Session.Session [Value]
materializeSnapshot = materializeSnapshotWithProfile FullV1

-- | The shell projection contains only the workspace record.  Navigation and
-- card data intentionally arrive through bounded HTTP contracts; global
-- snapshots retain their established full projection because a workspace shell
-- has no meaning there.
materializeSnapshotWithProfile :: SnapshotProfile -> ChangeScope -> Session.Session [Value]
materializeSnapshotWithProfile profile = \case
  WorkspaceScope workspace | profile == WorkspaceShellV1 -> Session.statement workspace workspaceShellSnapshotStatement
  WorkspaceScope workspace -> Session.statement workspace workspaceSnapshotStatement
  GlobalScope -> Session.statement () globalSnapshotStatement

workspaceShellSnapshotStatement :: Statement.Statement UUID [Value]
workspaceShellSnapshotStatement = Statement.Statement
  "SELECT jsonb_build_object('schema_version',1,'kind','workspace','data',jsonb_build_object('id',w.id,'name',w.name,'workspace_type',w.workspace_type::text,'gh_owner',w.gh_owner,'gh_repo',w.gh_repo,'created_at',w.created_at,'updated_at',w.updated_at)) FROM workspaces w WHERE w.id=$1 AND w.deleted_at IS NULL"
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) True

workspaceSnapshotStatement :: Statement.Statement UUID [Value]
workspaceSnapshotStatement = Statement.Statement workspaceSnapshotSql
  (Enc.param (Enc.nonNullable Enc.uuid))
  (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) True

-- Stable rank followed by identity is part of the wire contract.  All deleted
-- rows, embeddings, audit data, bearer state, and internal metadata are
-- deliberately absent.
workspaceSnapshotSql :: ByteString
workspaceSnapshotSql =
  "WITH snapshot_items(kind_rank, identity, item) AS ("
  <> " SELECT 10, w.id::text, jsonb_build_object('schema_version',1,'kind','workspace','data',jsonb_build_object('id',w.id,'name',w.name,'workspace_type',w.workspace_type::text,'gh_owner',w.gh_owner,'gh_repo',w.gh_repo,'created_at',w.created_at,'updated_at',w.updated_at)) FROM workspaces w WHERE w.id=$1 AND w.deleted_at IS NULL"
  <> " UNION ALL SELECT 20,p.id::text,jsonb_build_object('schema_version',1,'kind','project','data',jsonb_build_object('id',p.id,'workspace_id',p.workspace_id,'parent_id',p.parent_id,'name',p.name,'description',p.description,'status',p.status::text,'priority',p.priority,'metadata',p.metadata,'created_at',p.created_at,'updated_at',p.updated_at)) FROM projects p WHERE p.workspace_id=$1 AND p.deleted_at IS NULL"
  <> " UNION ALL SELECT 30,t.id::text,jsonb_build_object('schema_version',1,'kind','task','data',jsonb_build_object('id',t.id,'workspace_id',t.workspace_id,'project_id',t.project_id,'parent_id',t.parent_id,'title',t.title,'description',t.description,'status',t.status::text,'priority',t.priority,'metadata',t.metadata,'due_at',t.due_at,'completed_at',t.completed_at,'dependency_count',(SELECT count(*) FROM task_dependencies td WHERE td.task_id=t.id),'created_at',t.created_at,'updated_at',t.updated_at)) FROM tasks t WHERE t.workspace_id=$1 AND t.deleted_at IS NULL"
  <> " UNION ALL SELECT 40,td.task_id::text || ':' || td.depends_on_id::text,jsonb_build_object('schema_version',1,'kind','task_dependency','data',jsonb_build_object('task_id',td.task_id,'depends_on_id',td.depends_on_id)) FROM task_dependencies td WHERE td.workspace_id=$1"
  <> " UNION ALL SELECT 50,o.id::text,jsonb_build_object('schema_version',1,'kind','observation','data',jsonb_build_object('id',o.id,'workspace_id',o.workspace_id,'git_sha',o.git_sha,'content',o.content,'subjects',coalesce((SELECT jsonb_agg(jsonb_build_object('subject_kind',s.subject_kind::text,'subject',s.subject) ORDER BY s.ordinal) FROM observation_subjects s WHERE s.observation_id=o.id),'[]'::jsonb),'subject_kind',(SELECT s.subject_kind::text FROM observation_subjects s WHERE s.observation_id=o.id ORDER BY s.ordinal LIMIT 1),'subject',(SELECT s.subject FROM observation_subjects s WHERE s.observation_id=o.id ORDER BY s.ordinal LIMIT 1),'created_at',o.created_at,'updated_at',o.updated_at)) FROM observations o JOIN workspaces w ON w.id=o.workspace_id WHERE o.workspace_id=$1 AND w.workspace_type='repository' AND w.deleted_at IS NULL"
  <> " ) SELECT item FROM snapshot_items ORDER BY kind_rank, identity"

globalSnapshotStatement :: Statement.Statement () [Value]
globalSnapshotStatement = Statement.Statement globalSnapshotSql Enc.noParams
  (Dec.rowList (Dec.column (Dec.nonNullable Dec.jsonb))) True

globalSnapshotSql :: ByteString
globalSnapshotSql =
  "WITH snapshot_items(kind_rank, identity, item) AS ("
  <> " SELECT 10,w.id::text,jsonb_build_object('schema_version',1,'kind','workspace','data',jsonb_build_object('id',w.id,'name',w.name,'workspace_type',w.workspace_type::text,'gh_owner',w.gh_owner,'gh_repo',w.gh_repo,'created_at',w.created_at,'updated_at',w.updated_at)) FROM workspaces w WHERE w.deleted_at IS NULL"
  <> " UNION ALL SELECT 20,g.id::text,jsonb_build_object('schema_version',1,'kind','workspace_group','data',jsonb_build_object('id',g.id,'name',g.name,'description',g.description,'created_at',g.created_at,'updated_at',g.updated_at)) FROM workspace_groups g"
  <> " ) SELECT item FROM snapshot_items ORDER BY kind_rank, identity"
