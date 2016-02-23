module Overview.Component.Changes
  ( Model
  , init
  , update
  , Action
  , view
  )
  where

import Dict
import Effects as Fx
import Html exposing (..)
import Html.Attributes exposing (class, key, src, style)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy3)
import Json.Decode as Decode
import StartApp
import Task

import Docs.Entry as Entry
import Docs.Package as Docs
import Docs.Type as Type
import Docs.Version as Vsn
import Overview.Constants as Constants
import Overview.Diff as Diff
import Overview.History as History
import Overview.Slider as Slider
import Page.Context as Ctx
import Parse.Type as Type
import Utils.Path exposing ((</>))



-- MODEL


type alias Model =
    { history : History.History
    , docs : Dict.Dict Vsn.Version Docs
    }


type Docs
    = Loading
    | Failed Int
    | Ready (Docs.Package Type.Type)


init : Ctx.OverviewContext -> History.History -> ( Model, Fx.Effects Action )
init context history =
  ( Model
      history
      (Dict.fromList (List.map (\h -> (.version h) => Loading) history))
  , Fx.batch
      (List.map (loadDocs context << .version) history)
  )


-- UPDATE


type Action
    = DocsFailed Vsn.Version
    | DocsLoaded Vsn.Version (Docs.Package Type.Type)


update : Ctx.OverviewContext -> Action -> Model -> ( Model, Fx.Effects Action )
update context action model =
  case action of
    DocsFailed vsn ->
      ( { model | docs = Dict.insert vsn (Failed 1) model.docs }
      , Fx.none
      )

    DocsLoaded vsn vsnDocs ->
      ( { model | docs = Dict.insert vsn (Ready vsnDocs) model.docs }
      , Fx.none
      )


-- EFFECTS


loadDocs : Ctx.OverviewContext -> Vsn.Version -> Fx.Effects Action
loadDocs context version =
  Ctx.getDocs { user = context.user, project = context.project, version = Debug.log "version" <| Vsn.vsnToString version }
    |> Task.map (DocsLoaded version << Docs.packageMap Type.parseWithFallback)
    |> flip Task.onError (always (Task.succeed (DocsFailed version)))
    |> Fx.task


-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address {history, docs} =
  let
    diffView version1 version2 =
      div []
          [ diffHeader version1 version2
          , lazy3 coarseDiff history version1 version2
          , lazy3 detailedDiff docs version1 version2
          ]
    vs =
      List.map .version history

    v2s =
      List.drop 1 vs
  in
    div []
      (List.map2 diffView v2s vs)


diffHeader : Vsn.Version -> Vsn.Version -> Html
diffHeader version1 version2 =
  h1 []
     [ text (Vsn.vsnToString version1) ]


vsnText : String -> Vsn.Version -> Html
vsnText color vsn =
  span
    [ style
        [ "border-bottom" => ("4px solid " ++ color)
        ]
    ]
    [ text (Vsn.vsnToString vsn)
    ]


-- VIEW DIFFS


coarseDiff history version1 version2 =
  let
    {added, changed, removed} =
      History.diff version1 version2 history
  in
    div
      []
      [ coarseDiffChunk ("Added " ++ toString added)
      , coarseDiffChunk ("Changed " ++ toString changed)
      , coarseDiffChunk ("Removed " ++ toString removed)
      ]


coarseDiffChunk msg =
  span [ style [ "padding" => "0 16px" ] ] [ text msg ]


detailedDiff docs version1 version2 =
  let
    lowDocs =
      Dict.get (min version1 version2) docs

    highDocs =
      Dict.get (max version1 version2) docs
  in
    case Maybe.map2 (,) lowDocs highDocs of
      Just (Ready pkg1, Ready pkg2) ->
        detailedDiffHelp docs pkg1 pkg2

      Just (Loading, _) ->
        text "Loading.."

      Just (_, Loading) ->
        text "Loading.."

      _ ->
        text "UNEXPECTED STATE"


detailedDiffHelp docs pkg1 pkg2 =
  let
    { added, changed, removed } =
      Diff.diffPackage pkg1 pkg2
  in
    ul [class "diff-detailed"] <|
      List.map (listItem "diff-module-header" "removed") removed
      ++ List.map (listItem "diff-module-header" "added") added
      ++ viewPackageChanges changed


listItem itemClass verbed name =
  li [key name]
    [ header
        [class itemClass]
        [ code [] [text name], text (" was " ++ verbed ++ ".") ]
    ]


viewPackageChanges changes =
  List.map viewModuleChanges (Dict.toList changes)


viewModuleChanges (name, changes) =
  let
    changePairs =
      Dict.values changes.changed
  in
    li [key name]
      [ header [class "diff-module-header"] [ code [] [ text name ] ]
      , ul [class "diff-removed"] (List.map viewEntry (Dict.values changes.removed))
      , ul [class "diff-added"] (List.map viewEntry (Dict.values changes.added))
      , ul [class "diff-changed"] (List.map viewChangePair changePairs)
      ]


viewChangePair (old, new) =
  li []
    [ viewTypeAnnotation old "old"
    , br [] []
    , viewTypeAnnotation new "new"
    ]


viewEntry entry =
  li [] [viewTypeAnnotation entry ""]


viewTypeAnnotation entry class' =
  let
    kids =
      Entry.viewTypeAnnotation False Dict.empty entry
        |> List.intersperse [text "\n"]
        |> List.concat

  in
    span [class ("formatted-code " ++ class')] kids

