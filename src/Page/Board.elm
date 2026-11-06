module Page.Board exposing
    ( Model
    , Msg(..)
    , init
    , mapSession
    , toSession
    , update
    , view
    )

import Activity
import Board exposing (Board)
import BoardConfig exposing (BoardConfig)
import Boards exposing (Boards)
import Card exposing (Card)
import CardOrder
import Column exposing (Column)
import Columns
import Date exposing (Date)
import DragAndDrop.BeaconPosition as BeaconPosition exposing (BeaconPosition)
import DragAndDrop.Coords as Coords
import DragAndDrop.DragData as DragData exposing (DragData)
import DragAndDrop.DragTracker as DragTracker exposing (DragTracker)
import DragAndDrop.Rect as Rect
import FeatherIcons
import Html exposing (Attribute, Html)
import Html.Attributes exposing (attribute, checked, class, disabled, hidden, id, style, tabindex, type_)
import Html.Events exposing (on, onClick, preventDefaultOn, stopPropagationOn)
import Html.Events.Extra.Mouse as Mouse exposing (onContextMenu, onDown)
import Html.Keyed
import Html.Lazy
import InteropPorts
import Json.Decode as JD
import Json.Encode as JE
import List.Extra as LE
import Page.Helper.DatePicker as DatePicker exposing (DatePicker)
import Person
import SafeZipper
import Session exposing (Session, TaskCompletionSettings)
import SmartSchedule
import TagList
import TaskItem exposing (TaskItem, TaskItemFields)
import TaskList
import TextDirection
import Time
import TimeWithZone exposing (TimeWithZone)
import UpdatedTaskItem exposing (UpdatedTaskItem)



-- MODEL


type Model
    = DeletingCard String String Session
    | EditingCardDueDate DatePicker TaskItem Session
    | DraggingCard String Session
    | ViewingBoard Session


init : Session -> Model
init session =
    ViewingBoard session


cancelCurrentState : Model -> Model
cancelCurrentState model =
    ViewingBoard (toSession model)


deleteCardRequested : String -> String -> Model -> Model
deleteCardRequested title cardId model =
    DeletingCard title cardId (toSession model)


deleteCardConfirmed : Model -> Model
deleteCardConfirmed model =
    ViewingBoard (toSession model)


editCardDueDateConfirmed : Model -> Model
editCardDueDateConfirmed model =
    ViewingBoard (toSession model)


editCardDueDateRequested : String -> Model -> Model
editCardDueDateRequested cardId model =
    let
        taskItem : Maybe TaskItem
        taskItem =
            toSession model
                |> Session.findCard cardId
                |> Maybe.map Card.taskItem
    in
    case taskItem of
        Just taskToEdit ->
            let
                date : Maybe Date
                date =
                    TaskItem.due taskToEdit

                today : Date
                today =
                    toSession model
                        |> Session.timeWithZone
                        |> TimeWithZone.toDate

                firstDayOfWeek : Time.Weekday
                firstDayOfWeek =
                    toSession model
                        |> Session.firstDayOfWeek
            in
            EditingCardDueDate (DatePicker.init firstDayOfWeek today date) taskToEdit (toSession model)

        Nothing ->
            model


mapSession : (Session -> Session) -> Model -> Model
mapSession fn model =
    case model of
        DraggingCard taskId session ->
            DraggingCard taskId <| fn session

        ViewingBoard session ->
            ViewingBoard <| fn session

        DeletingCard title cardId session ->
            DeletingCard title cardId <| fn session

        EditingCardDueDate datePicker card session ->
            EditingCardDueDate datePicker card <| fn session


toSession : Model -> Session
toSession model =
    case model of
        DraggingCard _ session ->
            session

        ViewingBoard session ->
            session

        DeletingCard _ _ session ->
            session

        EditingCardDueDate _ _ session ->
            session



-- UPDATE


type Msg
    = ActivityChanged Activity.Msg
    | CardDragStarted String
    | CardDragEnded
    | CardDropped Int
    | CardDroppedAt { taskId : String, columnIndex : Int, sourceColumnIndex : Int, beforeTaskId : Maybe String }
    | AddCardClicked Int
    | MoveCardRequested String
    | AssignCardRequested String
    | EditPlanningRequested String
    | PersonSelected (Maybe String)
    | SmartScheduleToggled
    | ScheduleWeekChanged Int
    | CardMouseDown
    | CardRightMouseDown String Mouse.Event
    | ColumnMouseDown ( String, DragTracker.ClientData )
    | DatePickerMsg DatePicker.Msg
    | DeleteConfirmed String
    | EditCardDueDateConfirmed
    | EditCardDueDateRequested String
    | ElementDragged DragData
    | ModalCancelClicked
    | ModalCloseClicked
    | SettingsClicked
    | TabHeaderMouseDown ( String, DragTracker.ClientData )
    | TabSelected Int
    | TaskItemEditClicked String
    | TaskItemDeleteClicked String String
    | TaskItemToggled String
    | ToggleColumnCollapse Int Bool


tagHeaderDragType : String
tagHeaderDragType =
    "card-board-tag-header"


columnDragType : String
columnDragType =
    "card-board-column"


update : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update msg model =
    case msg of
        ActivityChanged activityMsg ->
            redrawSession (Session.updateActivity activityMsg) model

        CardDragStarted taskId ->
            ( DraggingCard taskId (toSession model), Cmd.none, Session.NoOp )

        CardDragEnded ->
            case model of
                DraggingCard _ session ->
                    ( ViewingBoard session, Cmd.none, Session.NoOp )

                _ ->
                    ( model, Cmd.none, Session.NoOp )

        CardDroppedAt data ->
            let
                session =
                    toSession model

                tasks =
                    Session.taskFromId data.taskId session

                config =
                    Session.boardConfigs session |> SafeZipper.current

                columns =
                    config |> Maybe.map (\board -> Board.init (Session.uniqueId session) board (Session.boardTaskList session) |> Board.columns (Session.ignoreFileNameDates session) (TimeWithZone.toDate (Session.timeWithZone session))) |> Maybe.withDefault []

                columnTasks =
                    columns |> List.drop data.columnIndex |> List.head |> Maybe.map (\column -> Column.cards "order" column |> CardOrder.sort (Session.globalSettings session).cardOrder (config |> Maybe.map BoardConfig.name |> Maybe.withDefault "") (Column.name column) |> List.map Card.taskItem) |> Maybe.withDefault []
            in
            ( ViewingBoard session
            , tasks |> Maybe.map (\task -> InteropPorts.reorderCard { boardIndex = boardIndexForSession session, columnIndex = data.columnIndex, sourceColumnIndex = data.sourceColumnIndex, taskItem = task, beforeTask = data.beforeTaskId |> Maybe.andThen (\id_ -> Session.taskFromId id_ session), columnTasks = columnTasks }) |> Maybe.withDefault Cmd.none
            , Session.NoOp
            )

        CardDropped columnIndex ->
            case model of
                DraggingCard taskId session ->
                    ( ViewingBoard session
                    , Session.taskFromId taskId session
                        |> Maybe.map (\taskItem -> InteropPorts.moveCard { boardIndex = boardIndexForSession session, columnIndex = columnIndex, taskItem = taskItem })
                        |> Maybe.withDefault Cmd.none
                    , Session.NoOp
                    )

                _ ->
                    ( model, Cmd.none, Session.NoOp )

        AddCardClicked columnIndex ->
            ( model
            , InteropPorts.createCard { boardIndex = boardIndexForSession (toSession model), columnIndex = columnIndex }
            , Session.NoOp
            )

        PersonSelected person ->
            redrawSession (Session.setPersonFilter person) model

        SmartScheduleToggled ->
            redrawSession Session.toggleSmartSchedule model

        ScheduleWeekChanged delta ->
            redrawSession (Session.changeScheduleWeek delta) model

        EditPlanningRequested taskId ->
            ( model
            , Session.taskFromId taskId (toSession model)
                |> Maybe.map (\taskItem -> InteropPorts.editPlanning { boardIndex = boardIndexForSession (toSession model), taskItem = taskItem })
                |> Maybe.withDefault Cmd.none
            , Session.NoOp
            )

        AssignCardRequested taskId ->
            ( model
            , Session.taskFromId taskId (toSession model)
                |> Maybe.map (\taskItem -> InteropPorts.assignCard { boardIndex = boardIndexForSession (toSession model), taskItem = taskItem, people = Session.taskList (toSession model) |> TaskList.topLevelTasks |> List.concatMap TaskItem.assignees |> LE.uniqueBy String.toLower |> List.sortBy String.toLower })
                |> Maybe.withDefault Cmd.none
            , Session.NoOp
            )

        MoveCardRequested taskId ->
            ( model
            , Session.taskFromId taskId (toSession model)
                |> Maybe.map (\taskItem -> InteropPorts.showMoveCardMenu { boardIndex = boardIndexForSession (toSession model), taskItem = taskItem })
                |> Maybe.withDefault Cmd.none
            , Session.NoOp
            )

        CardMouseDown ->
            ( model, Cmd.none, Session.NoOp )

        CardRightMouseDown cardId event ->
            ( model
            , InteropPorts.showCardContextMenu event.clientPos cardId
            , Session.NoOp
            )

        ColumnMouseDown ( domId, clientData ) ->
            ( mapSession (Session.waitForDrag clientData) model
            , InteropPorts.trackDraggable columnDragType clientData.clientPos domId
            , Session.NoOp
            )

        DatePickerMsg subMsg ->
            case model of
                EditingCardDueDate datePicker card session ->
                    let
                        newPicker : DatePicker
                        newPicker =
                            DatePicker.update subMsg datePicker
                    in
                    ( EditingCardDueDate newPicker card session
                    , Cmd.none
                    , Session.NoOp
                    )

                _ ->
                    ( model, Cmd.none, Session.NoOp )

        DeleteConfirmed cardId ->
            ( deleteCardConfirmed model
            , cmdIfHasTask cardId model InteropPorts.deleteTask
            , Session.NoOp
            )

        EditCardDueDateConfirmed ->
            case model of
                EditingCardDueDate datePicker taskItem session ->
                    let
                        cmd : Cmd Msg
                        cmd =
                            InteropPorts.rewriteTasks
                                (TaskItem.filePath taskItem)
                                updatedTaskItems

                        taskCompletionSettings : TaskCompletionSettings
                        taskCompletionSettings =
                            Session.taskCompletionSettings session

                        updatedTaskItems : List UpdatedTaskItem
                        updatedTaskItems =
                            taskItem
                                |> UpdatedTaskItem.init
                                |> UpdatedTaskItem.updateDate taskCompletionSettings (DatePicker.pickedDate datePicker)
                                |> List.singleton
                    in
                    ( editCardDueDateConfirmed model
                    , cmd
                    , Session.NoOp
                    )

                _ ->
                    ( model, Cmd.none, Session.NoOp )

        EditCardDueDateRequested cardId ->
            ( editCardDueDateRequested cardId model
            , Cmd.none
            , Session.NoOp
            )

        ElementDragged dragData ->
            case dragData.dragAction of
                DragData.Move ->
                    if dragData.dragType == tagHeaderDragType then
                        ( model
                            |> updateBoardOrder (Session.dragTracker <| toSession model) dragData
                            |> (mapSession <| Session.moveDragable dragData)
                        , Cmd.none
                        , Session.NoOp
                        )

                    else if dragData.dragType == columnDragType then
                        ( model
                            |> updateColumnOrder (Session.dragTracker <| toSession model) dragData
                            |> (mapSession <| Session.moveDragable dragData)
                        , displayGhostMarkdown model
                        , Session.NoOp
                        )

                    else
                        ( model, Cmd.none, Session.NoOp )

                DragData.Stop ->
                    ( mapSession Session.stopTrackingDragable model
                    , InteropPorts.updateSettings <| Session.settings <| toSession model
                    , Session.NoOp
                    )

        ModalCancelClicked ->
            ( cancelCurrentState model
            , Cmd.none
            , Session.NoOp
            )

        ModalCloseClicked ->
            ( cancelCurrentState model
            , Cmd.none
            , Session.NoOp
            )

        SettingsClicked ->
            ( model
            , Cmd.none
            , Session.SettingsClicked
            )

        TabHeaderMouseDown ( domId, clientData ) ->
            ( mapSession (Session.waitForDrag clientData) model
            , InteropPorts.trackDraggable tagHeaderDragType clientData.clientPos domId
            , Session.NoOp
            )

        TabSelected tabIndex ->
            redrawSession (Session.switchToBoardAt tabIndex) model

        TaskItemDeleteClicked title id ->
            ( deleteCardRequested title id model
            , Cmd.none
            , Session.NoOp
            )

        TaskItemEditClicked id ->
            ( model
            , cmdIfHasTask id model InteropPorts.openTaskSourceFile
            , Session.NoOp
            )

        TaskItemToggled id ->
            let
                cmd : Cmd Msg
                cmd =
                    session
                        |> Session.taskContainingId id
                        |> Maybe.map toggleCmd
                        |> Maybe.withDefault Cmd.none

                updatedTaskItems : TaskItem -> List UpdatedTaskItem
                updatedTaskItems taskItem =
                    taskItem
                        |> TaskItem.tasksToToggle id timeWithZone
                        |> List.map UpdatedTaskItem.init
                        |> List.map (UpdatedTaskItem.toggleCompletion taskCompletionSettings timeWithZone)

                session : Session
                session =
                    toSession model

                taskCompletionSettings : TaskCompletionSettings
                taskCompletionSettings =
                    Session.taskCompletionSettings session

                timeWithZone : TimeWithZone
                timeWithZone =
                    Session.timeWithZone session

                toggleCmd : TaskItem -> Cmd Msg
                toggleCmd taskItem =
                    InteropPorts.rewriteTasks
                        (TaskItem.filePath taskItem)
                        (updatedTaskItems taskItem)
            in
            ( model
            , cmd
            , Session.NoOp
            )

        ToggleColumnCollapse columnIndex newState ->
            let
                newSession : Session
                newSession =
                    Session.updateColumnCollapse columnIndex newState (toSession model)
            in
            ( mapSession (always newSession) model
            , InteropPorts.updateSettings <| Session.settings newSession
            , Session.NoOp
            )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        DraggingCard _ session ->
            Html.div [ class "card-board-dragging-card" ]
                [ boardsView session ]

        ViewingBoard session ->
            Html.div []
                [ boardsView session ]

        DeletingCard title cardId session ->
            Html.div []
                [ boardsView session
                , modalDeleteCardConfirm title cardId
                ]

        EditingCardDueDate datePicker taskItem session ->
            Html.div []
                [ boardsView session
                , modalEditCardDueDate datePicker taskItem session
                ]


modalEditCardDueDate : DatePicker -> TaskItem -> Session -> Html Msg
modalEditCardDueDate datePicker taskItem session =
    let
        containingColumns : List String
        containingColumns =
            case currentBoardConfig of
                Just boardConfig ->
                    Board.init "" boardConfig TaskList.empty
                        |> Board.columns ignoreFileNameDates today
                        |> Columns.fromList
                        |> Columns.addTaskItem today updatedTaskItem
                        |> Columns.toList
                        |> List.filter (\c -> Column.cardCount c /= 0)
                        |> List.map Column.name

                Nothing ->
                    []

        currentBoardConfig : Maybe BoardConfig
        currentBoardConfig =
            Session.boardConfigs session
                |> SafeZipper.current

        ignoreFileNameDates : Bool
        ignoreFileNameDates =
            Session.ignoreFileNameDates session

        isInvalid : Bool
        isInvalid =
            not <| DatePicker.isValid datePicker

        today : Date
        today =
            Session.timeWithZone session
                |> TimeWithZone.toDate

        updatedTaskItem : TaskItem
        updatedTaskItem =
            if isInvalid then
                taskItem

            else
                TaskItem.updateDueDate (DatePicker.pickedDate datePicker) taskItem
    in
    Html.div [ class "modal-container" ]
        [ Html.div
            [ class "modal-bg"
            , style "opacity" "0.85"
            ]
            []
        , Html.div
            [ class "modal"
            , class "edit-date"
            ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                []
            , Html.div [ class "modal-content" ]
                [ Html.div [ class "setting-item setting-item-heading" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text <| "Task: " ++ TaskItem.title taskItem ]
                        , Html.div [ class "setting-item-description" ]
                            []
                        ]
                    , Html.div [ class "setting-item-control" ] []
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Due date" ]
                        , Html.div [ class "setting-item-description" ]
                            [ Html.text "Use YYYY-MM-DD, due dd-mm-yy, or due dd-mm. Leave blank to clear the due date." ]
                        ]
                    , Html.div [ class "setting-item-control" ]
                        [ DatePicker.view datePicker
                            |> Html.map DatePickerMsg
                        ]
                    ]
                , Html.div [ class "setting-item" ]
                    [ Html.div [ class "setting-item-info" ]
                        [ Html.div [ class "setting-item-name" ]
                            [ Html.text "Containing columns" ]
                        , containingColumnsText containingColumns
                        ]
                    , containingColumnsList containingColumns
                    ]
                ]
            , Html.div [ class "modal-button-container" ]
                [ Html.button
                    [ attributeIf (not isInvalid) (class "mod-cta")
                    , attributeIf isInvalid (attribute "aria-disabled" "true")
                    , onClick EditCardDueDateConfirmed
                    , disabled isInvalid
                    , tabindex 2
                    ]
                    [ Html.text "Save"
                    ]
                , Html.button
                    [ onClick ModalCancelClicked
                    , tabindex 3
                    ]
                    [ Html.text "Cancel"
                    ]
                ]
            ]
        ]


containingColumnsText : List String -> Html Msg
containingColumnsText containingColumns =
    if List.length containingColumns == 0 then
        Html.div
            [ class "setting-item-description"
            , class "mod-warning"
            ]
            [ Html.text "If you save with this due date, this task will no longer appear on this board." ]

    else
        Html.div [ class "setting-item-description" ]
            [ Html.text "If you save with this due date, this task will appear in the following columns on this board." ]


containingColumnsList : List String -> Html Msg
containingColumnsList containingColumns =
    if List.length containingColumns == 0 then
        Html.text ""

    else
        Html.div
            [ class "multiselect-items"
            , class "mod-faux"
            ]
            (List.map columnNamePill containingColumns)


columnNamePill : String -> Html Msg
columnNamePill name =
    Html.div [ class "multiselect-item" ]
        [ Html.span [ class "multiselect-item-single" ]
            [ Html.text name ]
        ]


modalDeleteCardConfirm : String -> String -> Html Msg
modalDeleteCardConfirm title cardId =
    Html.div [ class "modal-container" ]
        [ Html.div
            [ class "modal-bg"
            , style "opacity" "0.85"
            ]
            []
        , Html.div [ class "modal" ]
            [ Html.div
                [ class "modal-close-button"
                , onClick ModalCloseClicked
                ]
                []
            , Html.div [ class "modal-title" ]
                [ Html.text "Delete Card" ]
            , Html.div [ class "modal-content" ]
                [ Html.p [ class "mod-warning" ]
                    [ Html.text <|
                        "Press Delete to confirm you wish to delete the \""
                            ++ title
                            ++ "\" card.  This does not delete the task from your "
                            ++ "markdown, it encloses it in <del> tags."
                    ]
                ]
            , Html.div [ class "modal-button-container" ]
                [ Html.button
                    [ class "mod-warning"
                    , onClick <| DeleteConfirmed cardId
                    , tabindex 1
                    ]
                    [ Html.text "Delete"
                    ]
                , Html.button
                    [ onClick <| ModalCancelClicked
                    , tabindex 2
                    ]
                    [ Html.text "Cancel"
                    ]
                ]
            ]
        ]


boardsView : Session -> Html Msg
boardsView session =
    if SafeZipper.length (Session.boardConfigs session) == 0 then
        Html.text "Loading tasks...."

    else
        let
            boards : Boards
            boards =
                Boards.init (Session.uniqueId session) (Session.boardConfigs session) (Session.visibleTaskList session)

            currentBoardIndex : Maybe Int
            currentBoardIndex =
                Boards.currentIndex boards

            ignoreFileNameDates : Bool
            ignoreFileNameDates =
                Session.ignoreFileNameDates session

            today : Date
            today =
                Session.timeWithZone session
                    |> TimeWithZone.toDate

            orders =
                (Session.globalSettings session).cardOrder |> List.map (\order -> { order | board = Session.uniqueId session ++ ":" ++ String.replace " " "_" order.board })

            isDragging : Bool
            isDragging =
                Session.isDragging session && draggedType == Just tagHeaderDragType

            dragTracker : DragTracker
            dragTracker =
                session
                    |> Session.dragTracker

            draggedType : Maybe String
            draggedType =
                DragTracker.dragType dragTracker
        in
        Html.div
            ([ attribute "dir" (TextDirection.toString <| Session.textDirection session)
             , attributeIf isDragging (style "cursor" "grabbing")
             ]
                ++ Person.colorAttributes (Session.globalSettings session).people
            )
            [ Html.div [ class "workspace-tab-header-container" ]
                [ Html.div
                    [ class "sidebar-toggle-button"
                    , class "mod-left"
                    , attribute "aria-label" "Settings"
                    , attribute "aria-label-position" "right"
                    ]
                    [ Html.div
                        [ class "clickable-icon"
                        , onClick SettingsClicked
                        ]
                        [ FeatherIcons.settings
                            |> FeatherIcons.withSize 1
                            |> FeatherIcons.withSizeUnit "em"
                            |> FeatherIcons.toHtml []
                        ]
                    ]
                , Html.div
                    [ class "workspace-tab-header-container-inner" ]
                    (tabHeaders isDragging currentBoardIndex boards
                        ++ [ viewDraggedHeader isDragging session ]
                    )
                , Html.div
                    [ class "card-board-tab-header-spacer" ]
                    []
                ]
            , boardControls session
            , if (Session.viewOptions session).activity.visible then
                Html.Lazy.lazy5 Activity.view (Session.timeWithZone session).zone today (Session.viewOptions session).activity (Session.viewOptions session).person (Session.taskList session |> TaskList.topLevelTasks) |> Html.map ActivityChanged

              else if (Session.viewOptions session).smart then
                smartScheduleView session

              else
                Html.Keyed.node "div"
                    [ class "card-board-boards"
                    , class "card-board-column-container"
                    ]
                    (Boards.boardZipper boards
                        |> SafeZipper.mapSelectedAndRest
                            (keyedSelectedBoardView orders ignoreFileNameDates today dragTracker)
                            (\board -> ( Board.id board, Html.text "" ))
                        |> SafeZipper.toList
                    )
            ]


tabHeaders : Bool -> Maybe Int -> Boards -> List (Html Msg)
tabHeaders isDragging currentBoardIndex boards =
    Boards.names boards
        |> SafeZipper.indexedMapSelectedAndRest (selectedTabHeader isDragging) (tabHeader isDragging currentBoardIndex)
        |> SafeZipper.toList


tabHeader : Bool -> Maybe Int -> Int -> String -> Html Msg
tabHeader isDragging currentBoardIndex tabIndex name =
    let
        headerClass : String
        headerClass =
            tabHeaderClass currentBoardIndex tabIndex

        domId : String
        domId =
            "card-board-tab:" ++ String.fromInt tabIndex
    in
    Html.div
        [ id domId
        , class ("workspace-tab-header" ++ headerClass)
        , attributeIf (not isDragging) (attribute "aria-label" name)
        , attributeIf (not isDragging) (attribute "aria-label-delay" "50")
        , onClick <| TabSelected tabIndex
        , onDown
            (\e ->
                TabHeaderMouseDown <|
                    ( domId
                    , { uniqueId = name
                      , clientPos = Coords.fromFloatTuple e.clientPos
                      , offsetPos = Coords.fromFloatTuple e.offsetPos
                      }
                    )
            )
        ]
        [ beacon tagHeaderBeaconType (BeaconPosition.Before name)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| name ]
            ]
        , beacon tagHeaderBeaconType (BeaconPosition.After name)
        ]


selectedTabHeader : Bool -> Int -> String -> Html Msg
selectedTabHeader isDragging tabIndex name =
    let
        domId : String
        domId =
            "card-board-tab:" ++ String.fromInt tabIndex
    in
    Html.div
        [ class "workspace-tab-header is-active"
        , id domId
        , attributeIf (not isDragging) (attribute "aria-label" name)
        , attributeIf isDragging (attribute "aria-label" "")
        , attributeIf (not isDragging) (attribute "aria-label-delay" "50")
        , attributeIf isDragging (style "opacity" "0.0")
        , onDown
            (\e ->
                TabHeaderMouseDown <|
                    ( domId
                    , { uniqueId = name
                      , clientPos = Coords.fromFloatTuple e.clientPos
                      , offsetPos = Coords.fromFloatTuple e.offsetPos
                      }
                    )
            )
        ]
        [ beacon tagHeaderBeaconType (BeaconPosition.Before name)
        , Html.div
            [ class "workspace-tab-header-inner" ]
            [ Html.div [ class "workspace-tab-header-inner-title" ]
                [ Html.text <| name ]
            ]
        , beacon tagHeaderBeaconType (BeaconPosition.After name)
        ]


viewDraggedHeader : Bool -> Session -> Html Msg
viewDraggedHeader isDragging session =
    case ( isDragging, Session.dragTracker session ) of
        ( True, DragTracker.Dragging clientData domData ) ->
            Html.div
                [ class "workspace-tab-header is-active"
                , id <| "card-board-tab:being-dragged"
                , style "position" "fixed"
                , style "top" (String.fromFloat (domData.draggedNodeStartRect.y - domData.offset.y) ++ "px")
                , style "left" (String.fromFloat (clientData.clientPos.x - domData.offset.x - clientData.offsetPos.x) ++ "px")
                , style "width" (String.fromFloat domData.draggedNodeStartRect.width ++ "px")
                , style "height" (String.fromFloat domData.draggedNodeStartRect.height ++ "px")
                , style "cursor" "grabbing"
                , style "opacity" "0.85"
                ]
                [ Html.div
                    [ class "workspace-tab-header-inner" ]
                    [ Html.div [ class "workspace-tab-header-inner-title" ]
                        [ Html.text <| clientData.uniqueId ]
                    ]
                ]

        _ ->
            empty


keyedBoardView : List CardOrder.Order -> Bool -> Date -> Board -> ( String, Html Msg )
keyedBoardView orders ignoreFileNameDates today board =
    ( Board.id board, Html.Lazy.lazy4 boardView orders ignoreFileNameDates today board )


boardView : List CardOrder.Order -> Bool -> Date -> Board -> Html Msg
boardView orders ignoreFileNameDates today board =
    Html.div
        [ class "card-board-board"
        , hidden True
        ]
        [ Html.Keyed.node "div"
            [ class "card-board-columns" ]
            (board
                |> Board.columns ignoreFileNameDates today
                |> List.indexedMap (\index column -> keyedColumnView orders Nothing (Board.id board) index today column)
            )
        ]


keyedSelectedBoardView : List CardOrder.Order -> Bool -> Date -> DragTracker -> Board -> ( String, Html Msg )
keyedSelectedBoardView orders ignoreFileNameDates today dragTracker board =
    ( Board.id board, Html.Lazy.lazy5 selectedBoardView orders ignoreFileNameDates today dragTracker board )


selectedBoardView : List CardOrder.Order -> Bool -> Date -> DragTracker -> Board -> Html Msg
selectedBoardView orders ignoreFileNameDates today dragTracker board =
    let
        draggedColumn : Maybe Column
        draggedColumn =
            board
                |> Board.columns ignoreFileNameDates today
                |> LE.find (\c -> Just (Column.name c) == draggedUniqueId)

        draggedType : Maybe String
        draggedType =
            DragTracker.dragType dragTracker

        draggedUniqueId : Maybe String
        draggedUniqueId =
            if isDragging then
                DragTracker.uniqueId dragTracker

            else
                Nothing

        isDragging : Bool
        isDragging =
            DragTracker.isDragging dragTracker && draggedType == Just columnDragType
    in
    Html.div [ class "card-board-board" ]
        [ Html.Keyed.node "div"
            [ class "card-board-columns" ]
            ((board
                |> Board.columns ignoreFileNameDates today
                |> List.indexedMap (\index column -> keyedColumnView orders draggedUniqueId (Board.id board) index today column)
             )
                |> (\cs ->
                        List.append cs
                            [ keyedColumnGhostView (Board.id board) today isDragging dragTracker draggedColumn ]
                   )
            )
        ]


keyedColumnGhostView : String -> Date -> Bool -> DragTracker -> Maybe Column -> ( String, Html Msg )
keyedColumnGhostView boardId today isDragging dragTracker draggedColumn =
    ( boardId ++ ":" ++ (Maybe.map Column.name draggedColumn |> Maybe.withDefault "") ++ ":ghost"
    , Html.Lazy.lazy5 columnGhostView boardId today isDragging dragTracker draggedColumn
    )


columnGhostView : String -> Date -> Bool -> DragTracker -> Maybe Column -> Html Msg
columnGhostView boardId today isDragging dragTracker draggedColumn =
    case ( isDragging, draggedColumn, dragTracker ) of
        ( True, Just column, DragTracker.Dragging clientData domData ) ->
            let
                name : String
                name =
                    Column.name column
            in
            Html.div []
                [ Html.div
                    [ class <| "card-board-column" ++ columnCollapsedClass column
                    , style "position" "fixed"
                    , style "top" (String.fromFloat (domData.draggedNodeStartRect.y - domData.offset.y) ++ "px")
                    , style "left" (String.fromFloat (clientData.clientPos.x - domData.offset.x - clientData.offsetPos.x) ++ "px")
                    , style "width" (String.fromFloat domData.draggedNodeStartRect.width ++ "px")
                    , style "height" (String.fromFloat domData.draggedNodeStartRect.height ++ "px")
                    , style "cursor" "grabbing"
                    , style "opacity" "0.85"
                    ]
                    [ Html.div [ class "card-board-column-header" ]
                        [ Html.div
                            [ class (columnCollapsedArrow column)
                            , attribute "aria-label" (columnCollapsedAria column)
                            ]
                            []
                        , Html.span []
                            [ Html.text <| name ]
                        , Html.span [ class "sub-text" ]
                            [ Html.text <| columnCountString column ]
                        ]
                    , Html.Keyed.ul [ class "card-board-column-list" ]
                        (List.map (cardView today) (Column.cards (ghostBoardId boardId) column))
                    ]
                ]

        _ ->
            Html.text ""


keyedColumnView : List CardOrder.Order -> Maybe String -> String -> Int -> Date -> Column -> ( String, Html Msg )
keyedColumnView orders draggedId boardId columnIndex today column =
    ( boardId ++ ":" ++ Column.name column
    , Html.Lazy.lazy6 columnView orders draggedId boardId columnIndex today column
    )


columnView : List CardOrder.Order -> Maybe String -> String -> Int -> Date -> Column -> Html Msg
columnView orders draggedId boardId columnIndex today column =
    let
        domId : String
        domId =
            "card-board-column:"
                ++ boardId
                ++ ":"
                ++ name

        isBeingDragged : Bool
        isBeingDragged =
            Just name == draggedId

        name : String
        name =
            Column.name column
    in
    Html.div
        [ class "card-board-column-with-beacons"
        , attribute "data-card-column-index" (String.fromInt columnIndex)
        , attribute "data-card-can-move"
            (if canMoveTo column then
                "true"

             else
                "false"
            )
        , on "cardboard-drop" (JD.field "detail" (JD.map4 (\taskId sourceColumnIndex columnIndex_ beforeTaskId -> CardDroppedAt { taskId = taskId, sourceColumnIndex = sourceColumnIndex, columnIndex = columnIndex_, beforeTaskId = beforeTaskId }) (JD.field "taskId" JD.string) (JD.field "sourceColumnIndex" JD.int) (JD.field "columnIndex" JD.int) (JD.field "beforeTaskId" (JD.nullable JD.string))))
        , attributeIf (canMoveTo column) (preventDefaultOn "dragover" (JD.succeed ( CardMouseDown, True )))
        , attributeIf (canMoveTo column) (preventDefaultOn "drop" (JD.succeed ( CardDropped columnIndex, True )))
        ]
        [ beacon columnBeaconType (BeaconPosition.Before name)
        , Html.div
            [ id domId
            , class <| "card-board-column" ++ columnCollapsedClass column
            , attributeIf isBeingDragged (style "opacity" "0")
            , onDown <|
                \e ->
                    ColumnMouseDown <|
                        ( domId
                        , { uniqueId = name
                          , clientPos = Coords.fromFloatTuple e.clientPos
                          , offsetPos = Coords.fromFloatTuple e.offsetPos
                          }
                        )
            ]
            [ Html.div [ class "card-board-column-header" ]
                [ Html.div
                    [ class (columnCollapsedArrow column)
                    , attribute "aria-label" (columnCollapsedAria column)
                    , onClick <| ToggleColumnCollapse columnIndex (not <| Column.isCollapsed column)
                    ]
                    []
                , Html.span []
                    [ Html.text <| name ]
                , Html.span [ class "sub-text" ]
                    [ Html.text <| columnCountString column ]
                ]
            , Html.Keyed.ul [ class "card-board-column-list" ]
                (Column.cards boardId column |> CardOrder.sort orders boardId (Column.name column) |> List.map (cardView today))
            , Html.button
                [ class "card-board-add-card"
                , onClick (AddCardClicked columnIndex)
                , stopPropagationOn "mousedown" (JD.succeed ( CardMouseDown, True ))
                , attribute "aria-label" ("Add card to " ++ name)
                ]
                [ Html.text "+ Add card" ]
                |> when (canMoveTo column && not (Column.isCollapsed column))
            ]
        , beacon columnBeaconType (BeaconPosition.After name)
        ]


cardView : Date -> Card -> ( String, Html Msg )
cardView today card =
    let
        cardId : String
        cardId =
            Card.id card

        dataTags : String
        dataTags =
            card
                |> Card.allTags
                |> TagList.toStrings
                |> List.map (String.replace "/" "-")
                |> String.join " "

        taskItem : TaskItem
        taskItem =
            Card.taskItem card

        taskItemId : String
        taskItemId =
            Card.taskItemId card

        highlightAreaClass : String
        highlightAreaClass =
            case Card.highlight today card of
                Card.HighlightCritical ->
                    "critical"

                Card.HighlightGood ->
                    "good"

                Card.HighlightImportant ->
                    "important"

                Card.HighlightNone ->
                    ""
    in
    Html.li
        [ class "card-board-card cm-s-obsidian"
        , attribute "data-assignee" (TaskItem.assignee taskItem |> Maybe.withDefault "")
        , attributeIf (not <| String.isEmpty dataTags) (attribute "data-tags" dataTags)
        , stopPropagationOn "mousedown" (JD.succeed ( CardMouseDown, True ))
        , attribute "data-task-id" taskItemId
        , attribute "draggable" "false"
        , on "dragstart" (JD.succeed (CardDragStarted taskItemId))
        , on "dragend" (JD.succeed CardDragEnded)
        , onContextMenu <| CardRightMouseDown (Card.id card)
        ]
        [ Html.div [ class ("card-board-card-highlight-area " ++ highlightAreaClass) ]
            []
        , Html.div
            [ class "card-board-card-content-area"
            , attribute "data-assignee-count" (String.fromInt (List.length (TaskItem.assignees taskItem)))
            ]
            (Person.background (TaskItem.assignees taskItem)
                :: [ Html.input
                        [ type_ "checkbox"
                        , class "task-list-item-checkbox"
                        , onClick <| TaskItemToggled taskItemId
                        , checked <| TaskItem.isCompleted taskItem
                        ]
                        []
                   , Html.div [ class "card-board-card-title", id cardId ]
                        []
                   , Html.div [ class "card-board-assignees" ]
                        (TaskItem.assignees taskItem |> List.map (\name -> Html.button [ class "card-board-assignee", onClick (AssignCardRequested taskItemId), attribute "title" "Change assignees or colours" ] [ Html.text name ]))
                   , Html.div [ class "card-board-task-meta" ]
                        [ TaskItem.estimateHours taskItem |> Maybe.map (\hours -> Html.span [] [ Html.text (String.fromFloat hours ++ "h estimated") ]) |> Maybe.withDefault (Html.text "")
                        , TaskItem.actualHours taskItem |> Maybe.map (\hours -> Html.span [] [ Html.text (String.fromFloat hours ++ "h spent") ]) |> Maybe.withDefault (Html.text "")
                        , if TaskItem.isUrgent taskItem then
                            Html.strong [ class "card-board-urgent" ] [ Html.text "Urgent" ]

                          else
                            Html.text ""
                        ]
                   , cardTagsView (Card.tagsId card)
                        |> when (TaskItem.hasTags taskItem)
                   , subtasksView (Card.descendantTasks card)
                        |> when (TaskItem.hasSubtasks taskItem)
                   , notesView (Card.notesId card)
                        |> when (TaskItem.hasNotes taskItem)
                   , Html.div [ class "card-board-card-footer-area" ]
                        [ taskDueDate today (TaskItem.due taskItem)
                            |> when (TaskItem.isDated taskItem)
                        , cardActionButtons (Card.title card) taskItemId (Card.editButtonId card)
                        ]
                   ]
            )
        ]
        |> Tuple.pair cardId


cardTagsView : String -> Html Msg
cardTagsView tagsId =
    Html.div [ class "card-board-card-tag-area", id tagsId ]
        []


notesView : String -> Html Msg
notesView notesId =
    Html.div [ class "card-board-card-notes-area", id notesId ]
        []


subtasksView : List ( String, TaskItem ) -> Html Msg
subtasksView subtasks =
    Html.div [ class "card-board-card-subtasks-area" ]
        [ Html.ul [ class "contains-task-list" ]
            (List.map subtaskView subtasks)
        ]


subtaskView : ( String, TaskItem ) -> Html Msg
subtaskView ( uniqueId, subtask ) =
    Html.li [ class "card-board-card-subtask task-list-item" ]
        [ Html.input
            [ type_ "checkbox"
            , class "task-list-item-checkbox"
            , onClick <| TaskItemToggled <| TaskItem.id subtask
            , checked <| TaskItem.isCompleted subtask
            ]
            []
        , Html.div [ class "card-board-card-title", id uniqueId ]
            []
        ]


taskDueDate : Date -> Maybe Date -> Html Msg
taskDueDate today dueDate =
    Html.div [ class "card-board-card-action-area-due" ]
        [ Html.text ("Due: " ++ dueDateString today dueDate)
        ]


cardActionButtons : String -> String -> String -> Html Msg
cardActionButtons title taskItemId editButtonId =
    Html.div [ class "card-board-card-action-area-buttons" ]
        [ Html.button
            [ class "card-board-card-action-area-button"
            , attribute "aria-label" "Edit estimate and urgency"
            , attribute "title" "Edit estimate and urgency"
            , onClick (EditPlanningRequested taskItemId)
            ]
            [ FeatherIcons.clock |> FeatherIcons.withSize 1 |> FeatherIcons.withSizeUnit "em" |> FeatherIcons.toHtml [] ]
        , Html.button
            [ class "card-board-card-action-area-button"
            , attribute "aria-label" "Assign person and colour"
            , attribute "title" "Assign person and colour"
            , onClick (AssignCardRequested taskItemId)
            ]
            [ FeatherIcons.user
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        , Html.button
            [ class "card-board-card-action-area-button"
            , attribute "aria-label" "Move card to column"
            , attribute "title" "Move card to column"
            , onClick (MoveCardRequested taskItemId)
            ]
            [ FeatherIcons.move
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        , Html.div
            [ class "card-board-card-action-area-button"
            , onClickWithPreventDefault <| TaskItemEditClicked taskItemId
            , id editButtonId
            ]
            [ FeatherIcons.edit
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        , Html.div
            [ class "card-board-card-action-area-button"
            , onClick <| TaskItemDeleteClicked title taskItemId
            ]
            [ FeatherIcons.trash
                |> FeatherIcons.withSize 1
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
            ]
        ]



-- HELPERS


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


attributeIf : Bool -> Attribute msg -> Attribute msg
attributeIf condition attribute =
    if condition then
        attribute

    else
        class ""


tagHeaderBeaconType : String
tagHeaderBeaconType =
    "data-" ++ tagHeaderDragType ++ "-beacon"


beacon : String -> BeaconPosition -> Html Msg
beacon beaconType beaconPosition =
    Html.span
        [ attribute beaconType (JE.encode 0 <| BeaconPosition.encoder beaconPosition)
        , style "font-size" "0"
        ]
        []


cmdIfHasTask : String -> Model -> (TaskItemFields -> Cmd b) -> Cmd b
cmdIfHasTask id model cmd =
    model
        |> toSession
        |> Session.taskFromId id
        |> Maybe.map TaskItem.fields
        |> Maybe.map cmd
        |> Maybe.withDefault Cmd.none


columnBeaconType : String
columnBeaconType =
    "data-" ++ columnDragType ++ "-beacon"


columnCollapsedAria : Column -> String
columnCollapsedAria column =
    if Column.isCollapsed column then
        "Un-collapse"

    else
        "Collapse"


columnCollapsedArrow : Column -> String
columnCollapsedArrow column =
    if Column.isCollapsed column then
        "arrow-down"

    else
        "arrow-right"


columnCollapsedClass : Column -> String
columnCollapsedClass column =
    if Column.isCollapsed column then
        " collapsed"

    else
        ""


columnCountString : Column -> String
columnCountString column =
    if Column.isCollapsed column then
        "(" ++ (String.fromInt <| Column.cardCount column) ++ ")"

    else
        ""


displayGhostMarkdown : Model -> Cmd Msg
displayGhostMarkdown model =
    let
        dragTracker : DragTracker
        dragTracker =
            Session.dragTracker session

        session : Session
        session =
            toSession model

        currentBoard : Maybe Board
        currentBoard =
            session
                |> Session.boardConfigs
                |> SafeZipper.current
                |> Maybe.map (\bc -> Board.init (Session.uniqueId session) bc (Session.taskList session))

        ghostId : String
        ghostId =
            currentBoard
                |> Maybe.map (ghostBoardId << Board.id)
                |> Maybe.withDefault ""

        ignoreFileNameDates : Bool
        ignoreFileNameDates =
            Session.ignoreFileNameDates session

        today : Date
        today =
            Session.timeWithZone session
                |> TimeWithZone.toDate
    in
    currentBoard
        |> Maybe.map (Board.columns ignoreFileNameDates today)
        |> Maybe.withDefault []
        |> LE.find (\c -> Just (Column.name c) == DragTracker.uniqueId dragTracker)
        |> Maybe.map (Column.cards ghostId)
        |> Maybe.withDefault []
        |> InteropPorts.displayTaskMarkdown


dueDateString : Date -> Maybe Date -> String
dueDateString today dueDate =
    case dueDate of
        Just date ->
            if Date.year date == Date.year today then
                Date.format "E, MMM ddd" date

            else
                Date.format "E, MMM ddd y" date

        Nothing ->
            "n/a"


empty : Html Msg
empty =
    Html.text ""


ghostBoardId : String -> String
ghostBoardId boardId =
    boardId ++ ":ghost"


nonPropogatingOnDown : (Mouse.Event -> Msg) -> Attribute Msg
nonPropogatingOnDown =
    { stopPropagation = True, preventDefault = True }
        |> Mouse.onWithOptions "mousedown"


onClickWithPreventDefault : msg -> Attribute msg
onClickWithPreventDefault msg =
    Html.Events.preventDefaultOn "click" (JD.map alwaysPreventDefault (JD.succeed msg))


tabHeaderClass : Maybe Int -> Int -> String
tabHeaderClass currentBoardIndex index =
    case currentBoardIndex of
        Just i ->
            if index == i - 1 then
                " is-before-active"

            else if index == i + 1 then
                " is-after-active"

            else
                ""

        Nothing ->
            ""


updateBoardOrder : DragTracker -> DragData -> Model -> Model
updateBoardOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo Coords.Horizontal cursor beacons of
                Nothing ->
                    model

                Just position ->
                    mapSession (Session.moveBoard clientData.uniqueId position) model

        _ ->
            model


updateColumnOrder : DragTracker -> DragData -> Model -> Model
updateColumnOrder dragTracker { cursor, beacons } model =
    case dragTracker of
        DragTracker.Dragging clientData _ ->
            case Rect.closestTo Coords.Horizontal cursor beacons of
                Nothing ->
                    model

                Just position ->
                    mapSession (Session.moveColumn clientData.uniqueId position) model

        _ ->
            model


when : Bool -> Html Msg -> Html Msg
when shouldRender html =
    if shouldRender then
        html

    else
        empty


boardIndexForSession : Session -> Int
boardIndexForSession session =
    Session.boardConfigs session |> SafeZipper.currentIndex |> Maybe.withDefault 0


canMoveTo : Column -> Bool
canMoveTo column =
    case column of
        Column.NamedTag _ ->
            True

        Column.Untagged _ ->
            True

        _ ->
            False


redrawSession : (Session -> Session) -> Model -> ( Model, Cmd Msg, Session.Msg )
redrawSession fn model =
    let
        updated =
            mapSession fn model

        cards =
            Session.cards (toSession updated)
    in
    ( updated, Cmd.batch [ InteropPorts.displayTaskMarkdown cards, InteropPorts.addHoverToCardEditButtons cards ], Session.NoOp )


boardControls : Session -> Html Msg
boardControls session =
    let
        options =
            Session.viewOptions session

        people =
            Session.taskList session |> TaskList.topLevelTasks |> List.concatMap TaskItem.assignees |> LE.uniqueBy String.toLower |> List.sortBy String.toLower

        tab name person =
            Html.button
                [ onClick (PersonSelected person)
                , attribute "role" "tab"
                , attribute "aria-selected"
                    (if options.person == person then
                        "true"

                     else
                        "false"
                    )
                ]
                [ Html.text name ]
    in
    Html.div [ class "card-board-controls" ]
        [ Html.div [ class "card-board-person-tabs", attribute "role" "tablist", attribute "aria-label" "Filter by person" ]
            (tab "All people" Nothing :: List.map (\name -> tab name (Just name)) people)
        , Html.button [ onClick (ActivityChanged Activity.ToggleView) ]
            [ Html.text
                (if options.activity.visible then
                    "Back to board"

                 else
                    "Activity view"
                )
            ]
        , Html.button
            [ onClick SmartScheduleToggled
            , attribute "aria-pressed"
                (if options.smart then
                    "true"

                 else
                    "false"
                )
            ]
            [ Html.text
                (if options.smart then
                    "Kanban view"

                 else
                    "Smart schedule view"
                )
            ]
        ]


smartScheduleView : Session -> Html Msg
smartScheduleView session =
    let
        today =
            Session.timeWithZone session |> TimeWithZone.toDate

        options =
            Session.viewOptions session

        schedule =
            Session.schedulePlan session

        boardId =
            Session.scheduleBoardId session

        slices =
            SmartSchedule.allocations today options.weekOffset schedule

        days =
            SmartSchedule.weekDays today options.weekOffset

        allocationView allocation =
            Html.li
                [ class
                    ("card-board-schedule-slot"
                        ++ (if allocation.late then
                                " is-late"

                            else
                                ""
                           )
                    )
                ]
                [ Html.div [ class "card-board-allocation-summary" ]
                    [ Html.text (String.fromFloat (toFloat (round (allocation.hours * 100)) / 100) ++ "h · " ++ allocation.person)
                    , if allocation.late then
                        Html.strong [] [ Html.text " · Scheduled after deadline" ]

                      else
                        Html.text ""
                    ]
                , Html.ul [ class "card-board-schedule-inner-list" ] [ SmartSchedule.allocationCard boardId allocation |> cardView today |> Tuple.second ]
                ]

        dayView day =
            Html.section [ class "card-board-schedule-day", attribute "aria-label" (Date.toIsoString day) ]
                [ Html.h3 [] [ Html.text (Date.format "EEE dd MMM" day) ]
                , Html.ul [ class "card-board-column-list" ] (slices |> List.filter (\allocation -> allocation.date == day) |> List.map allocationView)
                , if Date.toRataDie day < Date.toRataDie today then
                    Html.p [] [ Html.text "Past day" ]

                  else
                    Html.text ""
                ]

        pending title prefix tasks =
            if List.isEmpty tasks then
                Html.text ""

            else
                Html.section [ class "card-board-unscheduled" ]
                    [ Html.h3 [] [ Html.text (title ++ " (" ++ String.fromInt (List.length tasks) ++ ")") ]
                    , Html.ul [ class "card-board-column-list" ] (List.map (Card.fromTaskItem (boardId ++ prefix) [] >> cardView today >> Tuple.second) tasks)
                    ]
    in
    Html.div [ class "card-board-smart-schedule" ]
        [ Html.div [ class "card-board-week-controls" ]
            [ Html.button [ onClick (ScheduleWeekChanged -1), disabled (options.weekOffset == 0) ] [ Html.text "Previous week" ]
            , Html.span [] [ Html.text ("Week of " ++ (List.head days |> Maybe.map Date.toIsoString |> Maybe.withDefault "") ++ " · 8h/person/day") ]
            , Html.button [ onClick (ScheduleWeekChanged 1) ] [ Html.text "Next week" ]
            ]
        , Html.p [ class "card-board-schedule-rules" ] [ Html.text "Urgent first, then deadline, then shorter tasks. Undated tasks follow dated tasks within each urgency group. Starts today; split work continues across weekdays." ]
        , Html.div [ class "card-board-schedule-week" ] (List.map dayView days)
        , pending "Needs estimate" ":needs-estimate" schedule.needsEstimate
        , pending "Needs assignee" ":needs-assignee" schedule.needsAssignee
        ]
