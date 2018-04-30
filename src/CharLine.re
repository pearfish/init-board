type character = {
  id: string,
  name: string,
  bonus: int,
  init: float,
};

type state = {
  editText: string,
  editBonus: int,
  editing: bool,
  editFieldRef: ref(option(Dom.element)),
  editBonusFieldRef: ref(option(Dom.element)),
};

type action =
  | Edit
  | Submit
  | KeyDown(int)
  | ChangeName(string)
  | ChangeBonus(string);

let component = ReasonReact.reducerComponent("CharLineRe");

let setEditFieldRef = (r, {ReasonReact.state}) =>
  state.editFieldRef := Js.Nullable.toOption(r);

let setEditBonusFieldRef = (r, {ReasonReact.state}) =>
  state.editBonusFieldRef := Js.Nullable.toOption(r);

let parseBonus = bonus => {
  /* if (bonus == "-") {
    -0;
  } */

  let parsed =
  /* todo: this gets pretty fookin confused by attempts to type - */
    try (Some(int_of_string(String.trim(bonus)))) {
      | Failure(_) => None
    };

  switch parsed {
    | Some(n) => n
    | _ => 0
  };
};

let make =
    (
      ~character,
      ~isCurrent,
      ~editing,
      ~onDestroy,
      ~onSave,
      ~onEdit,
      ~onCancel,
      _children,
    ) => {
  let submitHelper = state =>
    switch (String.trim(state.editText)) {
    | "" => ReasonReact.SideEffects((_self => onDestroy()))
    | nonEmptyValue =>
      ReasonReact.UpdateWithSideEffects(
        {...state, editText: nonEmptyValue},
        (_self => onSave(nonEmptyValue, state.editBonus)),
      )
    };
  {
    ...component,
    initialState: () => {
      editText: character.name,
      editBonus: character.bonus,
      editFieldRef: ref(None),
      editBonusFieldRef: ref(None),
      editing,
    },
    reducer: action =>
      switch (action) {
      | Edit => (
          state => ReasonReact.Update({...state, editText: character.name})
        )
      | Submit => submitHelper
      | ChangeName(text) => (
          state =>
            editing ?
              ReasonReact.Update({...state, editText: text}) :
              ReasonReact.NoUpdate
        )
      | ChangeBonus(bonus) => (
          state =>
            editing ?
              ReasonReact.Update({...state, editBonus: parseBonus(bonus)}) :
              ReasonReact.NoUpdate
      )
      | KeyDown(27) =>
        onCancel();
        (state => ReasonReact.Update({...state, editText: character.name, editBonus: character.bonus}));
      | KeyDown(13) => submitHelper
      | KeyDown(_) => (_state => ReasonReact.NoUpdate)
      },
    willReceiveProps: ({state}) => {...state, editing},
    didUpdate: ({oldSelf, newSelf}) =>
      switch (oldSelf.state.editing, editing, newSelf.state.editFieldRef^) {
      | (false, true, Some(field)) =>
        let node = ReactDOMRe.domElementToObj(field);
        ignore(node##focus());
        ignore(
          node##setSelectionRange(node##value##length, node##value##length),
        );
      | _ => ()
      },
    /* escape key */
    render: ({state, handle, send}) => {
      let className =
        [editing ? "editing" : ""]
        |> String.concat(" ");

      let button = editing
          ? <button
            onClick=((_event) => send(Submit))
          >
            (ReasonReact.stringToElement("ok"))
          </button>
          : ReasonReact.nullElement;

      let main = editing
        ? <div className="editing">
            <input
              ref=(handle(setEditFieldRef))
              className="edit"
              value=state.editText
              onChange=(
                event =>
                  send(
                    ChangeName(
                      ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
                    ),
                  )
              )
              onKeyDown=(
                event => send(KeyDown(ReactEventRe.Keyboard.which(event)))
              )
            />
            <input
              ref=(handle(setEditBonusFieldRef))
              className="edit"
              value=string_of_int(state.editBonus)
              onChange=(
                event =>
                  send(
                    ChangeBonus(
                      ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
                    ),
                  )
              )
              onKeyDown=(
                event => send(KeyDown(ReactEventRe.Keyboard.which(event)))
              )
            />
          </div>
          : <div className="view">
            <label
              onDoubleClick=(
                _event => {
                  onEdit();
                  send(Edit);
                }
              )>
              (ReasonReact.stringToElement(character.name))
            </label>
            <div className="stat">
              (ReasonReact.stringToElement(string_of_float(character.init)))
            </div>
            <button className="destroy" onClick=((_) => onDestroy()) />
          </div>;

      let currentIndicator = isCurrent
        ? <div className="current-indicator">
          <img src="../assets/img/indicator-arrow.png" />
        </div>
        : ReasonReact.nullElement;

      <li className>
        (main)
        (button)
        (currentIndicator)
      </li>;
    },
  };
};
