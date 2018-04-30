type character = {
  id: string,
  name: string,
  bonus: int,
  init: float
};

type state = {
  editText: string,
  editBonus: string,
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
  let parsed = try (Some(int_of_string(String.trim(bonus)))) {
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
        (_self => onSave(nonEmptyValue, parseBonus(state.editBonus))),
      )
    };
  {
    ...component,
    initialState: () => {
      editText: character.name,
      editBonus: string_of_int(character.bonus),
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
      | ChangeBonus(editBonus) => (
          state =>
            editing ?
              ReasonReact.Update({...state, editBonus}) :
              ReasonReact.NoUpdate
      )
      | KeyDown(27) =>
        onCancel();
        (state => ReasonReact.Update({...state, editText: character.name, editBonus: string_of_int(character.bonus)}));
      | KeyDown(13) => submitHelper
      | KeyDown(_) => (_state => ReasonReact.NoUpdate)
      },
    willReceiveProps: ({state}) => {...state, editing, editBonus: string_of_int(character.bonus)},
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
    render: ({state, handle, send}) => {
      let className = editing ? "editing" : "";

      let main = editing
        ? <div className="editing">
            <input
              ref=(handle(setEditFieldRef))
              className="edit name"
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
              className="edit bonus"
              value=state.editBonus
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
            <button
              onClick=((_event) => send(Submit))
            >
              (ReasonReact.stringToElement("ok"))
            </button>
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
        (currentIndicator)
      </li>;
    },
  };
};
