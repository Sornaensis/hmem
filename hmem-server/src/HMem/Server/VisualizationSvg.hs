module HMem.Server.VisualizationSvg
  ( SVG
  , SvgDocument(..)
  , WorkspaceVisualizationResponse(..)
  , renderWorkspaceVisualizationSvg
  , svgAttachmentName
  ) where

import Data.Aeson (ToJSON(..))
import Data.Char (isAlphaNum)
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Media ((//), (/:))
import Servant (Accept(..), MimeRender(..))

import HMem.Types

data SVG

instance Accept SVG where
  contentType _ = "image" // "svg+xml" /: ("charset", "utf-8")

newtype SvgDocument = SvgDocument { unSvgDocument :: Text }
  deriving (Show, Eq)

instance MimeRender SVG SvgDocument where
  mimeRender _ (SvgDocument svgText) = BL.fromStrict (TE.encodeUtf8 svgText)

data WorkspaceVisualizationResponse = WorkspaceVisualizationResponse
  { responseQuery :: WorkspaceVisualizationQuery
  , responseVisualization :: WorkspaceVisualization
  }

instance ToJSON WorkspaceVisualizationResponse where
  toJSON response = toJSON response.responseVisualization

instance MimeRender SVG WorkspaceVisualizationResponse where
  mimeRender _ response =
    case renderWorkspaceVisualizationSvg response.responseQuery response.responseVisualization of
      SvgDocument svgText -> BL.fromStrict (TE.encodeUtf8 svgText)

data NodeBox = NodeBox
  { boxX      :: Double
  , boxY      :: Double
  , boxWidth  :: Double
  , boxHeight :: Double
  }

data ProjectPlacement = ProjectPlacement
  { placementProject :: Project
  , placementDepth   :: Int
  , placementBox     :: NodeBox
  }

data TaskPlacement = TaskPlacement
  { placementTask :: Task
  , placementBox  :: NodeBox
  }

data MemoryPlacement = MemoryPlacement
  { placementMemory :: VisualizationMemory
  , placementLinked :: Bool
  , placementBox    :: NodeBox
  }

data MemoryEdgeKind = DirectProjectMemory | TaskDerivedMemory
  deriving (Show, Eq, Ord)

data EffectiveMemoryEdge = EffectiveMemoryEdge
  { edgeProjectId :: Text
  , edgeMemoryId  :: Text
  , edgeKind      :: MemoryEdgeKind
  }
  deriving (Show, Eq, Ord)

renderWorkspaceVisualizationSvg :: WorkspaceVisualizationQuery -> WorkspaceVisualization -> SvgDocument
renderWorkspaceVisualizationSvg query visualization = SvgDocument $ T.unlines
  [ svgHeader canvasWidth canvasHeight
  , renderStyles
  , "  <defs>"
  , "    <linearGradient id=\"bg\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"100%\">"
  , "      <stop offset=\"0%\" stop-color=\"#f7fbff\"/>"
  , "      <stop offset=\"100%\" stop-color=\"#eef6ef\"/>"
  , "    </linearGradient>"
  , "    <filter id=\"shadow\" x=\"-10%\" y=\"-10%\" width=\"140%\" height=\"140%\">"
  , "      <feDropShadow dx=\"0\" dy=\"6\" stdDeviation=\"8\" flood-color=\"#1f2937\" flood-opacity=\"0.12\"/>"
  , "    </filter>"
  , "  </defs>"
  , "  <rect width=\"100%\" height=\"100%\" fill=\"url(#bg)\"/>"
  , renderWorkspaceHeader visualization.workspace showTasks showTaskStatusSummary headerWidth headerBoxHeight totalTaskCount directLinkCount derivedLinkCount memoryRelationCount globalTaskStatusSummary
  , sectionLabel padding (headerHeight + 8) "Projects"
  , T.concat [sectionLabel taskColumnX (headerHeight + 8) "Tasks" | showTasks]
  , sectionLabel memoryColumnX (headerHeight + 8) "Memories"
  , T.concat projectHierarchyEdges
  , T.concat projectTaskEdges
  , T.concat projectMemoryEdges
  , T.concat taskMemoryEdges
  , T.concat taskDependencyEdges
  , T.concat memoryRelationEdges
  , T.concat renderedProjects
  , T.concat renderedTasks
  , T.concat renderedMemories
  , "</svg>"
  ]
  where
    showTasks = fromMaybe False query.showTasks
    showTaskStatusSummary = fromMaybe True query.showTaskStatusSummary
    padding = 40
    headerBoxHeight = if showTaskStatusSummary then 104 else 84
    headerHeight = if showTaskStatusSummary then 136 else 116
    projectBoxWidth = 280
    projectBoxHeight = if showTaskStatusSummary then 92 else 74
    taskBoxWidth = 292
    taskBoxHeight = 66
    memoryBoxWidth = 296
    memoryBoxHeight = 72
    rowGap = 18
    depthGap = 24
    columnGap = 64

    projectMap = Map.fromList [(projectKey project.id, project) | project <- visualization.projects]
    childMap = Map.fromListWith (<>)
      [ (parentKey, [project])
      | project <- visualization.projects
      , Just parentId <- [project.parentId]
      , let parentKey = projectKey parentId
      ]
    rootProjects = sortProjects
      [ project
      | project <- visualization.projects
      , maybe True (\parentId -> Map.notMember (projectKey parentId) projectMap) project.parentId
      ]
    orderedProjects = concatMap (flattenProjectTree childMap 0) rootProjects
    maxDepth = maximumOrZero [placement.placementDepth | placement <- placements]
    projectAreaRight = padding + fromIntegral maxDepth * (projectBoxWidth + depthGap) + projectBoxWidth
    placements =
      [ ProjectPlacement
          { placementProject = project
          , placementDepth = depth
          , placementBox = NodeBox
              { boxX = padding + fromIntegral depth * (projectBoxWidth + depthGap)
              , boxY = headerHeight + 28 + fromIntegral index * (projectBoxHeight + rowGap)
              , boxWidth = projectBoxWidth
              , boxHeight = projectBoxHeight
              }
          }
      | (index, (project, depth)) <- zip [(0 :: Int) ..] orderedProjects
      ]
    projectBoxes = Map.fromList
      [ (projectKey placement.placementProject.id, placement.placementBox)
      | placement <- placements
      ]

    taskMap = Map.fromList [(taskKey task.id, task) | task <- visualization.tasks]
    taskCounts = Map.fromListWith (+)
      [ (projectKey projectId, 1 :: Int)
      | task <- visualization.tasks
      , Just projectId <- [task.projectId]
      ]
    tasksByProject = Map.fromListWith (<>)
      [ (projectKey projectId, [task])
      | task <- visualization.tasks
      , Just projectId <- [task.projectId]
      ]
    taskMemoryCounts = Map.fromListWith (+)
      [ (taskKey link.taskId, 1 :: Int)
      | link <- visualization.taskMemoryLinks
      ]
    globalTaskStatusSummary = if showTaskStatusSummary then Just (formatTaskStatusSummary visualization.tasks) else Nothing
    orderedTasks =
      [ task
      | project <- map fst orderedProjects
      , task <- sortTasks (Map.findWithDefault [] (projectKey project.id) tasksByProject)
      ]
      <> sortTasks [task | task <- visualization.tasks, task.projectId == Nothing]
    taskColumnX = if showTasks then projectAreaRight + columnGap else projectAreaRight + columnGap
    taskPlacements =
      [ TaskPlacement
          { placementTask = task
          , placementBox = NodeBox
              { boxX = taskColumnX
              , boxY = headerHeight + 28 + fromIntegral index * (taskBoxHeight + rowGap)
              , boxWidth = taskBoxWidth
              , boxHeight = taskBoxHeight
              }
          }
      | showTasks
      , (index, task) <- zip [(0 :: Int) ..] orderedTasks
      ]
    taskBoxes = Map.fromList
      [ (taskKey placement.placementTask.id, placement.placementBox)
      | placement <- taskPlacements
      ]

    directEdges = Set.fromList
      [ EffectiveMemoryEdge
          { edgeProjectId = projectKey link.projectId
          , edgeMemoryId = memoryKey link.memoryId
          , edgeKind = DirectProjectMemory
          }
      | link <- visualization.projectMemoryLinks
      , Map.member (projectKey link.projectId) projectBoxes
      ]
    derivedEdges = Set.fromList
      [ EffectiveMemoryEdge
          { edgeProjectId = projectKey projectId
          , edgeMemoryId = memoryKey link.memoryId
          , edgeKind = TaskDerivedMemory
          }
      | link <- visualization.taskMemoryLinks
      , Just task <- [Map.lookup (taskKey link.taskId) taskMap]
      , Just projectId <- [task.projectId]
      , Map.member (projectKey projectId) projectBoxes
      , not (Set.member
          EffectiveMemoryEdge
            { edgeProjectId = projectKey projectId
            , edgeMemoryId = memoryKey link.memoryId
            , edgeKind = DirectProjectMemory
            }
          directEdges)
      ]
    effectiveEdges = sortOn effectiveEdgeSortKey (Set.toList (directEdges <> derivedEdges))
    linkedMemoryIds = Set.fromList
      ([memoryKey link.memoryId | link <- visualization.projectMemoryLinks]
      <> [memoryKey link.memoryId | link <- visualization.taskMemoryLinks])

    sortedMemoryEntries =
      sortOn memoryEntrySortKey
        [ (memory, Set.member (memoryKey memory.id) linkedMemoryIds)
        | memory <- sortOn memorySortKey visualization.memories
        ]
    orderedMemories =
      [ MemoryPlacement
          { placementMemory = memory
          , placementLinked = linked
          , placementBox = NodeBox
              { boxX = memoryColumnX
              , boxY = headerHeight + 28 + fromIntegral index * (memoryBoxHeight + rowGap)
              , boxWidth = memoryBoxWidth
              , boxHeight = memoryBoxHeight
              }
          }
      | (index, (memory, linked)) <- zip [(0 :: Int) ..] sortedMemoryEntries
      ]
    memoryColumnX = if showTasks then taskColumnX + taskBoxWidth + columnGap else projectAreaRight + columnGap
    memoryBoxes = Map.fromList
      [ (memoryKey placement.placementMemory.id, placement.placementBox)
      | placement <- orderedMemories
      ]

    projectBottom = maximumOr (headerHeight + 28) [placement.placementBox.boxY + placement.placementBox.boxHeight | placement <- placements]
    taskBottom = maximumOr (headerHeight + 28) [placement.placementBox.boxY + placement.placementBox.boxHeight | placement <- taskPlacements]
    memoryBottom = maximumOr (headerHeight + 28) [placement.placementBox.boxY + placement.placementBox.boxHeight | placement <- orderedMemories]
    canvasWidth = memoryColumnX + memoryBoxWidth + padding
    canvasHeight = maximum [projectBottom + padding, taskBottom + padding, memoryBottom + padding]
    headerWidth = canvasWidth - 48

    renderedProjects =
      [ renderProjectNode showTaskStatusSummary placement (Map.findWithDefault 0 (projectKey placement.placementProject.id) taskCounts) (countProjectMemories (projectKey placement.placementProject.id) effectiveEdges) (Map.lookup (projectKey placement.placementProject.id) projectStatusSummaries)
      | placement <- placements
      ]
    renderedTasks =
      [ renderTaskNode placement (Map.findWithDefault 0 (taskKey placement.placementTask.id) taskMemoryCounts)
      | placement <- taskPlacements
      ]
    renderedMemories = [renderMemoryNode placement | placement <- orderedMemories]
    projectStatusSummaries = fmap formatTaskStatusSummary tasksByProject

    projectHierarchyEdges =
      [ renderProjectHierarchyEdge parentBox childBox
      | placement <- placements
      , Just parentId <- [placement.placementProject.parentId]
      , Just parentBox <- [Map.lookup (projectKey parentId) projectBoxes]
      , let childBox = placement.placementBox
      ]
    projectMemoryEdges =
      [ renderProjectMemoryEdge edge.edgeKind projectBox memoryBox
      | edge <- if showTasks then sortOn effectiveEdgeSortKey (Set.toList directEdges) else effectiveEdges
      , Just projectBox <- [Map.lookup edge.edgeProjectId projectBoxes]
      , Just memoryBox <- [Map.lookup edge.edgeMemoryId memoryBoxes]
      ]
    projectTaskEdges =
      [ renderProjectTaskEdge projectBox taskBox
      | showTasks
      , placement <- taskPlacements
      , Just projectId <- [placement.placementTask.projectId]
      , Just projectBox <- [Map.lookup (projectKey projectId) projectBoxes]
      , let taskBox = placement.placementBox
      ]
    taskMemoryEdges =
      [ renderTaskMemoryEdge taskBox memoryBox
      | showTasks
      , link <- visualization.taskMemoryLinks
      , Just taskBox <- [Map.lookup (taskKey link.taskId) taskBoxes]
      , Just memoryBox <- [Map.lookup (memoryKey link.memoryId) memoryBoxes]
      ]
    taskDependencyEdges =
      [ renderTaskDependencyEdge taskBox dependencyBox
      | showTasks
      , dependency <- visualization.taskDependencies
      , Just taskBox <- [Map.lookup (taskKey dependency.taskId) taskBoxes]
      , Just dependencyBox <- [Map.lookup (taskKey dependency.dependsOnId) taskBoxes]
      ]
    memoryRelationEdges =
      [ renderMemoryRelationEdge relation sourceBox targetBox
      | relation <- visualization.memoryLinks
      , Just sourceBox <- [Map.lookup (memoryKey relation.sourceId) memoryBoxes]
      , Just targetBox <- [Map.lookup (memoryKey relation.targetId) memoryBoxes]
      ]

    totalTaskCount = length visualization.tasks
    directLinkCount = length [() | edge <- effectiveEdges, edge.edgeKind == DirectProjectMemory]
    derivedLinkCount = length [() | edge <- effectiveEdges, edge.edgeKind == TaskDerivedMemory]
    memoryRelationCount = length memoryRelationEdges

flattenProjectTree :: Map.Map Text [Project] -> Int -> Project -> [(Project, Int)]
flattenProjectTree childMap depth project =
  (project, depth)
    : concatMap (flattenProjectTree childMap (depth + 1)) (sortProjects (Map.findWithDefault [] (projectKey project.id) childMap))

sortProjects :: [Project] -> [Project]
sortProjects = sortOn (\project -> (Down project.priority, project.name))

sortTasks :: [Task] -> [Task]
sortTasks = sortOn (\task -> (taskStatusSortKey task.status, Down task.priority, task.createdAt, task.title))

taskStatusSortKey :: TaskStatus -> Int
taskStatusSortKey status = case status of
  Todo -> 0
  InProgress -> 1
  Blocked -> 2
  Done -> 3
  Cancelled -> 4

renderStyles :: Text
renderStyles = T.unlines
  [ "  <style>"
  , "    .title { font: 700 28px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #102a43; }"
  , "    .subtitle { font: 400 13px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #52606d; }"
  , "    .section-label { font: 700 14px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #243b53; letter-spacing: 0.06em; text-transform: uppercase; }"
  , "    .project-name { font: 700 15px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #102a43; }"
  , "    .project-meta { font: 400 12px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #486581; }"
  , "    .task-name { font: 700 14px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #102a43; }"
  , "    .task-meta { font: 400 12px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #486581; }"
  , "    .memory-summary { font: 600 13px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #102a43; }"
  , "    .memory-meta { font: 400 12px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #5c6b7a; }"
  , "    .badge { font: 700 11px 'Segoe UI', 'Helvetica Neue', sans-serif; fill: #334e68; }"
  , "    .edge-project { stroke: #9fb3c8; stroke-width: 2.5; fill: none; }"
  , "    .edge-project-task { stroke: #7b8794; stroke-width: 1.8; fill: none; opacity: 0.85; }"
  , "    .edge-memory-direct { stroke: #2f855a; stroke-width: 2.2; fill: none; }"
  , "    .edge-memory-task { stroke: #dd6b20; stroke-width: 2; fill: none; stroke-dasharray: 7 5; }"
  , "    .edge-task-dependency { stroke: #486581; stroke-width: 1.3; fill: none; stroke-dasharray: 4 4; opacity: 0.8; }"
  , "    .edge-memory-graph { stroke: #7f5af0; stroke-width: 1.5; fill: none; stroke-dasharray: 3 4; opacity: 0.85; }"
  , "  </style>"
  ]

renderWorkspaceHeader :: Workspace -> Bool -> Bool -> Double -> Double -> Int -> Int -> Int -> Int -> Maybe Text -> Text
renderWorkspaceHeader workspace showTasks showTaskStatusSummary headerWidth headerBoxHeight totalTasks directLinks derivedLinks memoryLinksCount mTaskSummary = T.unlines $
  [ T.concat ["  <title>", escapeXml workspace.name, " workspace visualization</title>"]
  , T.concat ["  <desc>Workspace visualization for ", escapeXml workspace.name, " showing ", descriptionText, ".</desc>"]
  , T.concat ["  <rect x=\"24\" y=\"20\" rx=\"24\" ry=\"24\" width=\"", svgNumber headerWidth, "\" height=\"", svgNumber headerBoxHeight, "\" fill=\"#ffffff\" opacity=\"0.92\" filter=\"url(#shadow)\"/>"]
  , T.concat ["  <text x=\"40\" y=\"56\" class=\"title\">", escapeXml workspace.name, "</text>"]
  , T.concat ["  <text x=\"40\" y=\"82\" class=\"subtitle\">", escapeXml (workspaceTypeToText workspace.workspaceType), maybe "" (\pathText -> " | " <> escapeXml pathText) workspace.path, "</text>"]
  , T.concat ["  <text x=\"540\" y=\"54\" class=\"subtitle\">Tasks in filter: ", T.pack (show totalTasks), "</text>"]
  , T.concat ["  <text x=\"540\" y=\"76\" class=\"subtitle\">Direct memory links: ", T.pack (show directLinks), " | via tasks: ", T.pack (show derivedLinks), " | memory relations: ", T.pack (show memoryLinksCount), "</text>"]
  ] <> [T.concat ["  <text x=\"540\" y=\"98\" class=\"subtitle\">", escapeXml summaryText, "</text>"] | showTaskStatusSummary, Just summaryText <- [mTaskSummary]]
  where
    descriptionText = if showTasks then "projects, tasks, and memories" else "projects and memories"

sectionLabel :: Double -> Double -> Text -> Text
sectionLabel x y label =
  T.concat ["  <text x=\"", svgNumber x, "\" y=\"", svgNumber y, "\" class=\"section-label\">", escapeXml label, "</text>"]

renderProjectNode :: Bool -> ProjectPlacement -> Int -> Int -> Maybe Text -> Text
renderProjectNode showTaskStatusSummary placement taskCount memoryCount mStatusSummary = T.unlines $
  [ T.concat ["  <rect x=\"", svgNumber box.boxX, "\" y=\"", svgNumber box.boxY, "\" width=\"", svgNumber box.boxWidth, "\" height=\"", svgNumber box.boxHeight, "\" rx=\"20\" ry=\"20\" fill=\"#ffffff\" stroke=\"#9fb3c8\" stroke-width=\"1.2\" filter=\"url(#shadow)\"/>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 18), "\" y=\"", svgNumber (box.boxY + 28), "\" class=\"project-name\">", escapeXml project.name, "</text>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 18), "\" y=\"", svgNumber (box.boxY + 49), "\" class=\"project-meta\">", escapeXml (projectStatusToText project.status), " | priority ", T.pack (show project.priority), " | depth ", T.pack (show placement.placementDepth), "</text>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 18), "\" y=\"", svgNumber (box.boxY + 67), "\" class=\"project-meta\">", T.pack (show taskCount), " tasks | ", T.pack (show memoryCount), " memory links</text>"]
  ] <> [T.concat ["  <text x=\"", svgNumber (box.boxX + 18), "\" y=\"", svgNumber (box.boxY + 85), "\" class=\"project-meta\">", escapeXml (truncateLabel 42 summaryText), "</text>"] | showTaskStatusSummary, Just summaryText <- [mStatusSummary]]
  where
    box = placement.placementBox
    project = placement.placementProject

renderTaskNode :: TaskPlacement -> Int -> Text
renderTaskNode placement memoryCount = T.unlines
  [ T.concat ["  <rect x=\"", svgNumber box.boxX, "\" y=\"", svgNumber box.boxY, "\" width=\"", svgNumber box.boxWidth, "\" height=\"", svgNumber box.boxHeight, "\" rx=\"18\" ry=\"18\" fill=\"#ffffff\" stroke=\"", taskStatusColor task.status, "\" stroke-width=\"1.8\" filter=\"url(#shadow)\"/>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 16), "\" y=\"", svgNumber (box.boxY + 26), "\" class=\"task-name\">", escapeXml (truncateLabel 40 task.title), "</text>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 16), "\" y=\"", svgNumber (box.boxY + 48), "\" class=\"task-meta\">", escapeXml (formatTaskMeta task memoryCount), "</text>"]
  ]
  where
    box = placement.placementBox
    task = placement.placementTask

renderMemoryNode :: MemoryPlacement -> Text
renderMemoryNode placement = T.unlines
  [ T.concat ["  <rect x=\"", svgNumber box.boxX, "\" y=\"", svgNumber box.boxY, "\" width=\"", svgNumber box.boxWidth, "\" height=\"", svgNumber box.boxHeight, "\" rx=\"18\" ry=\"18\" fill=\"", fillColor, "\" stroke=\"", strokeColor, "\" stroke-width=\"", strokeWidth, "\" filter=\"url(#shadow)\"/>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 18), "\" y=\"", svgNumber (box.boxY + 28), "\" class=\"memory-summary\">", escapeXml (truncateLabel 46 memory.summary), "</text>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + 18), "\" y=\"", svgNumber (box.boxY + 49), "\" class=\"memory-meta\">", escapeXml (memoryTypeToText memory.memoryType), " | importance ", T.pack (show memory.importance), if memory.pinned then " | pinned" else "", "</text>"]
  , T.concat ["  <text x=\"", svgNumber (box.boxX + box.boxWidth - 70), "\" y=\"", svgNumber (box.boxY + 20), "\" class=\"badge\">", if placement.placementLinked then "linked" else "workspace", "</text>"]
  ]
  where
    box = placement.placementBox
    memory = placement.placementMemory
    fillColor = if placement.placementLinked then "#ffffff" else "#f8fafc"
    strokeColor = if memory.pinned then "#dd6b20" else "#9aa5b1"
    strokeWidth = if memory.pinned then "2.2" else "1.1"

renderProjectHierarchyEdge :: NodeBox -> NodeBox -> Text
renderProjectHierarchyEdge parentBox childBox =
  T.concat
    [ "  <path class=\"edge-project\" d=\"M ", svgNumber (parentBox.boxX + parentBox.boxWidth), " ", svgNumber (parentBox.boxY + parentBox.boxHeight / 2)
    , " C ", svgNumber (parentBox.boxX + parentBox.boxWidth + 24), " ", svgNumber (parentBox.boxY + parentBox.boxHeight / 2)
    , ", ", svgNumber (childBox.boxX - 24), " ", svgNumber (childBox.boxY + childBox.boxHeight / 2)
    , ", ", svgNumber childBox.boxX, " ", svgNumber (childBox.boxY + childBox.boxHeight / 2), "\"/>"
    ]

renderProjectTaskEdge :: NodeBox -> NodeBox -> Text
renderProjectTaskEdge projectBox taskBox =
  T.concat
    [ "  <path class=\"edge-project-task\" d=\"M ", svgNumber (projectBox.boxX + projectBox.boxWidth), " ", svgNumber (projectBox.boxY + projectBox.boxHeight / 2)
    , " C ", svgNumber (projectBox.boxX + projectBox.boxWidth + 22), " ", svgNumber (projectBox.boxY + projectBox.boxHeight / 2)
    , ", ", svgNumber (taskBox.boxX - 22), " ", svgNumber (taskBox.boxY + taskBox.boxHeight / 2)
    , ", ", svgNumber taskBox.boxX, " ", svgNumber (taskBox.boxY + taskBox.boxHeight / 2), "\"/>"
    ]

renderProjectMemoryEdge :: MemoryEdgeKind -> NodeBox -> NodeBox -> Text
renderProjectMemoryEdge edgeKind projectBox memoryBox =
  T.concat
    [ "  <path class=\"", edgeClass, "\" d=\"M ", svgNumber (projectBox.boxX + projectBox.boxWidth), " ", svgNumber (projectBox.boxY + projectBox.boxHeight / 2)
    , " C ", svgNumber (projectBox.boxX + projectBox.boxWidth + 28), " ", svgNumber (projectBox.boxY + projectBox.boxHeight / 2)
    , ", ", svgNumber (memoryBox.boxX - 28), " ", svgNumber (memoryBox.boxY + memoryBox.boxHeight / 2)
    , ", ", svgNumber memoryBox.boxX, " ", svgNumber (memoryBox.boxY + memoryBox.boxHeight / 2), "\"/>"
    ]
  where
    edgeClass = case edgeKind of
      DirectProjectMemory -> "edge-memory-direct"
      TaskDerivedMemory -> "edge-memory-task"

renderTaskMemoryEdge :: NodeBox -> NodeBox -> Text
renderTaskMemoryEdge taskBox memoryBox =
  T.concat
    [ "  <path class=\"edge-memory-task\" d=\"M ", svgNumber (taskBox.boxX + taskBox.boxWidth), " ", svgNumber (taskBox.boxY + taskBox.boxHeight / 2)
    , " C ", svgNumber (taskBox.boxX + taskBox.boxWidth + 24), " ", svgNumber (taskBox.boxY + taskBox.boxHeight / 2)
    , ", ", svgNumber (memoryBox.boxX - 24), " ", svgNumber (memoryBox.boxY + memoryBox.boxHeight / 2)
    , ", ", svgNumber memoryBox.boxX, " ", svgNumber (memoryBox.boxY + memoryBox.boxHeight / 2), "\"/>"
    ]

renderTaskDependencyEdge :: NodeBox -> NodeBox -> Text
renderTaskDependencyEdge taskBox dependencyBox =
  T.concat
    [ "  <path class=\"edge-task-dependency\" d=\"M ", svgNumber (taskBox.boxX + taskBox.boxWidth / 2), " ", svgNumber taskBox.boxY
    , " C ", svgNumber (taskBox.boxX + taskBox.boxWidth / 2), " ", svgNumber (taskBox.boxY - 20)
    , ", ", svgNumber (dependencyBox.boxX + dependencyBox.boxWidth / 2), " ", svgNumber (dependencyBox.boxY + dependencyBox.boxHeight + 20)
    , ", ", svgNumber (dependencyBox.boxX + dependencyBox.boxWidth / 2), " ", svgNumber (dependencyBox.boxY + dependencyBox.boxHeight), "\"/>"
    ]

renderMemoryRelationEdge :: MemoryLink -> NodeBox -> NodeBox -> Text
renderMemoryRelationEdge relation sourceBox targetBox =
  T.concat
    [ "  <path class=\"edge-memory-graph\" d=\"M ", svgNumber (sourceBox.boxX + sourceBox.boxWidth), " ", svgNumber (sourceBox.boxY + sourceBox.boxHeight / 2)
    , " C ", svgNumber (sourceBox.boxX + sourceBox.boxWidth + 18), " ", svgNumber (sourceBox.boxY + sourceBox.boxHeight / 2)
    , ", ", svgNumber (targetBox.boxX + 18), " ", svgNumber (targetBox.boxY + targetBox.boxHeight / 2)
    , ", ", svgNumber targetBox.boxX, " ", svgNumber (targetBox.boxY + targetBox.boxHeight / 2), "\"><title>"
    , escapeXml (relationTypeToText relation.relationType), "</title></path>"
    ]

svgHeader :: Double -> Double -> Text
svgHeader width height =
  T.concat
    [ "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"", svgNumber width, "\" height=\"", svgNumber height
    , "\" viewBox=\"0 0 ", svgNumber width, " ", svgNumber height, "\">"
    ]

svgAttachmentName :: Workspace -> Text
svgAttachmentName workspace =
  "attachment; filename=\"" <> sanitizeFileStem workspace.name <> "-workspace-visualization.svg\""

sanitizeFileStem :: Text -> Text
sanitizeFileStem nameText =
  let normalized = T.map normalizeChar (T.toLower nameText)
      compact = T.intercalate "-" (filter (not . T.null) (T.splitOn "-" normalized))
  in if T.null compact then "workspace" else compact
  where
    normalizeChar ch
      | isAlphaNum ch = ch
      | otherwise = '-'

projectKey :: Show a => a -> Text
projectKey = T.pack . show

taskKey :: Show a => a -> Text
taskKey = T.pack . show

memoryKey :: Show a => a -> Text
memoryKey = T.pack . show

countProjectMemories :: Text -> [EffectiveMemoryEdge] -> Int
countProjectMemories projectId edges =
  Set.size $ Set.fromList [edge.edgeMemoryId | edge <- edges, edge.edgeProjectId == projectId]

formatTaskMeta :: Task -> Int -> Text
formatTaskMeta task memoryCount =
  T.intercalate " | " $ baseParts <> memoryParts
  where
    baseParts = [taskStatusLabel task.status, "priority " <> T.pack (show task.priority)]
    memoryParts
      | memoryCount > 0 = [T.pack (show memoryCount) <> " memory " <> pluralize "link" memoryCount]
      | otherwise = []

formatTaskStatusSummary :: [Task] -> Text
formatTaskStatusSummary tasks =
  case parts of
    [] -> "0 tasks"
    _ -> T.intercalate " | " parts
  where
    parts =
      [ T.pack (show count) <> " " <> label
      | (status, label) <-
          [ (Todo, "todo")
          , (InProgress, "in progress")
          , (Blocked, "blocked")
          , (Done, "completed")
          , (Cancelled, "cancelled")
          ]
      , let count = length [task | task <- tasks, task.status == status]
      , count > 0
      ]

taskStatusLabel :: TaskStatus -> Text
taskStatusLabel status = case status of
  Todo -> "todo"
  InProgress -> "in progress"
  Blocked -> "blocked"
  Done -> "completed"
  Cancelled -> "cancelled"

taskStatusColor :: TaskStatus -> Text
taskStatusColor status = case status of
  Todo -> "#2b6cb0"
  InProgress -> "#b7791f"
  Blocked -> "#c53030"
  Done -> "#2f855a"
  Cancelled -> "#718096"

pluralize :: Text -> Int -> Text
pluralize label count
  | count == 1 = label
  | otherwise = label <> "s"

memorySortKey :: VisualizationMemory -> (Down Bool, Down Int, Text)
memorySortKey memory = (Down memory.pinned, Down memory.importance, memory.summary)

memoryEntrySortKey :: (VisualizationMemory, Bool) -> (Down Bool, Down Bool, Down Int, Text)
memoryEntrySortKey (memory, linked) =
  ( Down linked
  , Down memory.pinned
  , Down memory.importance
  , memory.summary
  )

effectiveEdgeSortKey :: EffectiveMemoryEdge -> (Text, MemoryEdgeKind, Text)
effectiveEdgeSortKey edge = (edge.edgeProjectId, edge.edgeKind, edge.edgeMemoryId)

maximumOrZero :: [Int] -> Int
maximumOrZero [] = 0
maximumOrZero xs = maximum xs

maximumOr :: Double -> [Double] -> Double
maximumOr fallback [] = fallback
maximumOr _ xs = maximum xs

truncateLabel :: Int -> Text -> Text
truncateLabel maxChars label
  | T.length label <= maxChars = label
  | maxChars <= 3 = T.take maxChars label
  | otherwise = T.take (maxChars - 3) label <> "..."

escapeXml :: Text -> Text
escapeXml = T.concatMap escapeChar
  where
    escapeChar ch = case ch of
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '"' -> "&quot;"
      '\'' -> "&apos;"
      _ -> T.singleton ch

svgNumber :: Double -> Text
svgNumber value =
  case T.stripSuffix ".0" textValue of
    Just trimmed -> trimmed
    Nothing -> textValue
  where
    rounded = fromIntegral (round (value * 10) :: Int) / 10 :: Double
    textValue = T.pack (show rounded)