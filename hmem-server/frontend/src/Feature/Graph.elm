module Feature.Graph exposing (init, update, viewGraphPage)

import Api
import Dict
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import Ports exposing (initCytoscape)
import Toast exposing (addToast)
import Types exposing (..)


init : GraphModel
init =
    { visualization = Nothing
    , loaded = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotVisualization result ->
            case result of
                Ok viz ->
                    let
                        currentGraph =
                            model.graph

                        updatedGraph =
                            { currentGraph | visualization = Just viz, loaded = True }

                        newModel =
                            { model | graph = updatedGraph }
                    in
                    ( newModel, initCytoscapeGraph newModel )

                Err err ->
                    let
                        errorMsg =
                            case err of
                                Http.BadBody body ->
                                    "Decode error: " ++ body

                                Http.BadStatus code ->
                                    "HTTP " ++ String.fromInt code

                                Http.BadUrl u ->
                                    "Bad URL: " ++ u

                                Http.Timeout ->
                                    "Request timed out"

                                Http.NetworkError ->
                                    "Network error"
                    in
                    addToast Error ("Graph load failed: " ++ errorMsg) model

        CytoscapeNodeClicked _ ->
            ( model, Cmd.none )

        CytoscapeEdgeClicked _ ->
            ( model, Cmd.none )

        LoadGraphForWorkspace wsId ->
            let
                currentGraph =
                    model.graph

                updatedGraph =
                    { currentGraph | loaded = False, visualization = Nothing }
            in
            ( { model
                | selectedWorkspaceId = Just wsId
                , graph = updatedGraph
              }
            , Api.fetchVisualization model.flags.apiUrl wsId GotVisualization
            )

        _ ->
            ( model, Cmd.none )



-- CYTOSCAPE GRAPH


initCytoscapeGraph : Model -> Cmd Msg
initCytoscapeGraph model =
    case model.graph.visualization of
        Nothing ->
            Cmd.none

        Just viz ->
            let
                projectNodes =
                    viz.projects
                        |> List.map
                            (\p ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string p.id )
                                            , ( "label", Encode.string (truncateText 40 p.name) )
                                            ]
                                      )
                                    , ( "classes", Encode.string "project" )
                                    ]
                            )

                taskNodes =
                    viz.tasks
                        |> List.map
                            (\t ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string t.id )
                                            , ( "label", Encode.string (truncateText 40 t.title) )
                                            ]
                                      )
                                    , ( "classes", Encode.string "task" )
                                    ]
                            )

                memNodes =
                    viz.memories
                        |> List.map
                            (\m ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string m.id )
                                            , ( "label", Encode.string (truncateText 40 m.summary) )
                                            ]
                                      )
                                    , ( "classes", Encode.string "memory" )
                                    ]
                            )

                memoryLinkEdges =
                    viz.memoryLinks
                        |> List.map
                            (\link ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("ml-" ++ link.sourceId ++ "-" ++ link.targetId) )
                                            , ( "source", Encode.string link.sourceId )
                                            , ( "target", Encode.string link.targetId )
                                            , ( "label", Encode.string link.relationType )
                                            ]
                                      )
                                    ]
                            )

                projectMemoryEdges =
                    viz.projectMemoryLinks
                        |> List.map
                            (\link ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("pm-" ++ link.projectId ++ "-" ++ link.memoryId) )
                                            , ( "source", Encode.string link.projectId )
                                            , ( "target", Encode.string link.memoryId )
                                            ]
                                      )
                                    , ( "classes", Encode.string "entity-memory" )
                                    ]
                            )

                taskMemoryEdges =
                    viz.taskMemoryLinks
                        |> List.map
                            (\link ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("tm-" ++ link.taskId ++ "-" ++ link.memoryId) )
                                            , ( "source", Encode.string link.taskId )
                                            , ( "target", Encode.string link.memoryId )
                                            ]
                                      )
                                    , ( "classes", Encode.string "entity-memory" )
                                    ]
                            )

                taskDependencyEdges =
                    viz.taskDependencies
                        |> List.map
                            (\dep ->
                                Encode.object
                                    [ ( "data"
                                      , Encode.object
                                            [ ( "id", Encode.string ("td-" ++ dep.taskId ++ "-" ++ dep.dependsOnId) )
                                            , ( "source", Encode.string dep.taskId )
                                            , ( "target", Encode.string dep.dependsOnId )
                                            ]
                                      )
                                    , ( "classes", Encode.string "dependency" )
                                    ]
                            )

                allElements =
                    projectNodes ++ taskNodes ++ memNodes ++ memoryLinkEdges ++ projectMemoryEdges ++ taskMemoryEdges ++ taskDependencyEdges

                positionsKey =
                    case model.selectedWorkspaceId of
                        Just wsId ->
                            graphPositionsKey wsId

                        Nothing ->
                            ""
            in
            initCytoscape
                (Encode.object
                    [ ( "containerId", Encode.string "cytoscape-container" )
                    , ( "elements", Encode.list identity allElements )
                    , ( "positionsKey", Encode.string positionsKey )
                    ]
                )



-- GRAPH PAGE


viewGraphPage : Model -> Html Msg
viewGraphPage model =
    div [ class "page" ]
        [ div [ class "page-header" ]
            [ h2 [] [ text "Knowledge Graph" ]
            , viewGraphWorkspaceSelector model
            ]
        , case model.graph.visualization of
            Nothing ->
                case model.selectedWorkspaceId of
                    Nothing ->
                        div [ class "empty-state" ] [ text "Select a workspace to view its knowledge graph." ]

                    Just _ ->
                        if model.graph.loaded then
                            div [ class "empty-state" ] [ text "No data found for this workspace." ]

                        else
                            div [ class "loading-indicator" ] [ text "Loading graph..." ]

            Just _ ->
                div [ id "cytoscape-container", style "width" "100%", style "height" "calc(100vh - 8rem)" ] []
        ]


viewGraphWorkspaceSelector : Model -> Html Msg
viewGraphWorkspaceSelector model =
    let
        wsList =
            Dict.values model.workspaces |> List.sortBy .name
    in
    if List.isEmpty wsList then
        text ""

    else
        select
            [ class "form-input"
            , style "width" "auto"
            , style "max-width" "250px"
            , onInput
                (\s ->
                    if String.isEmpty s then
                        NoOp

                    else
                        LoadGraphForWorkspace s
                )
            ]
            (option [ value "", selected (model.selectedWorkspaceId == Nothing) ] [ text "Select workspace..." ]
                :: List.map
                    (\ws ->
                        option [ value ws.id, selected (model.selectedWorkspaceId == Just ws.id) ]
                            [ text ws.name ]
                    )
                    wsList
            )
