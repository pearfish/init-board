open Belt;

[@bs.val] external unsafeJsonParse : string => 'a = "JSON.parse";

let localStorageNamespace = "init-board";

let saveLocally = xs =>
  switch (Js.Json.stringifyAny(xs)) {
  | None => ()
  | Some(stringifiedXs) =>
    Dom.Storage.(
      localStorage |> setItem(localStorageNamespace, stringifiedXs)
    )
  };

module Top = {
  type action =
    | NewTodoEnterKeyDown
    | NewTodoOtherKeyDown
    | Next
    | Cancel
    | ChangeTodo(string)
    | Save(CharLine.character, string, int)
    | Edit(CharLine.character)
    | Destroy(CharLine.character)
    | RerollAll;
  type state = {
    editing: option(string),
    newTodo: string,
    current: int,
    characters: list(CharLine.character),
  };
  let component = ReasonReact.reducerComponent("InitBoardRe");
  let make = _children => {
    ...component,
    reducer: (action, state) =>
      switch (action) {
      | Cancel => ReasonReact.Update({...state, editing: None})
      | ChangeTodo(text) => ReasonReact.Update({...state, newTodo: text})
      | NewTodoOtherKeyDown => ReasonReact.NoUpdate
      | NewTodoEnterKeyDown =>
        switch (String.trim(state.newTodo)) {
        | "" => ReasonReact.NoUpdate
        | nonEmptyValue =>
          let characters =
            state.characters
            @ [
              {
                id: string_of_float(Js.Date.now()),
                name: nonEmptyValue,
                bonus: 3,
                init: 13.3
              },
            ];
          saveLocally(characters);
          ReasonReact.Update({...state, newTodo: "", characters});
        }
      | Next => ReasonReact.Update({...state, current: (state.current + 1) mod List.length(state.characters)})
      | RerollAll =>
        let characters =
          List.map(state.characters, c =>
            {...c, CharLine.init: float_of_int(Random.int(20) + 1 + c.bonus) +. (float_of_int(c.bonus) /. 10.0)}
          );
        ReasonReact.UpdateWithSideEffects(
          {...state, characters, current: 0},
          (_self => saveLocally(characters)),
        )
      | Save(characterToSave, text, bonus) =>
        let characters =
          List.map(state.characters, c =>
            c == characterToSave ? {...c, CharLine.name: text, CharLine.bonus: bonus} : c
          );
        ReasonReact.UpdateWithSideEffects(
          {...state, editing: None, characters},
          (_self => saveLocally(characters)),
        );
      | Edit(todo) =>
        ReasonReact.Update({...state, editing: Some(CharLine.(todo.id))})
      | Destroy(todo) =>
        let characters = List.keep(state.characters, candidate => candidate !== todo);
        ReasonReact.UpdateWithSideEffects(
          {...state, characters},
          (_self => saveLocally(characters)),
        );
      },
    initialState: () => {
      let characters =
        switch (Dom.Storage.(localStorage |> getItem(localStorageNamespace))) {
        | None => []
        | Some(characters) => unsafeJsonParse(characters)
        };
      {
        editing: None,
        newTodo: "",
        current: -1,
        characters,
      };
    },
    /* router actions */
    render: ({state, send}) => {
      let {characters, current, editing} = state;

      let sortedChars = characters
        |> List.sort(_, (a, b) => int_of_float(b.init -. a.init))
        |> List.mapWithIndex(
             _,
             (i, character) => {
               let editing =
                 switch (editing) {
                 | None => false
                 | Some(editing) => editing === CharLine.(character.id)
                 };
               <CharLine
                 key=character.id
                 character
                 isCurrent={current == i}
                 onDestroy=(_event => send(Destroy(character)))
                 onEdit=(_event => send(Edit(character)))
                 editing
                 onSave=((text, bonus) => send(Save(character, text, bonus)))
                 onCancel=(_event => send(Cancel))
               />;
             },
           );
      let characterCount = List.length(sortedChars);

      let main =
        characterCount === 0 ?
          ReasonReact.nullElement :
          <section className="main">
            <ul className="char-list">
              (ReasonReact.arrayToElement(List.toArray(sortedChars)))
            </ul>
          </section>;

      <div>
        <header className="header">
          <h1> (ReasonReact.stringToElement("initiative")) </h1>
          <input
            className="new-char"
            placeholder="new character / monster name"
            value=state.newTodo
            onKeyDown=(
              event =>
                if (ReactEventRe.Keyboard.keyCode(event) === 13) {
                  ReactEventRe.Keyboard.preventDefault(event);
                  send(NewTodoEnterKeyDown);
                } else {
                  send(NewTodoOtherKeyDown);
                }
            )
            onChange=(
              event =>
                send(
                  ChangeTodo(
                    ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value,
                  ),
                )
            )
            autoFocus=true
          />
          <button
            className="button reroll"
            onClick=(_event => send(RerollAll))
          >
            (ReasonReact.stringToElement("reroll all"))
          </button>
          <button
            className="button reroll"
            onClick=(_event => send(Next))
          >
            (ReasonReact.stringToElement("next"))
          </button>
        </header>
        main
      </div>;
    },
  };
};

ReactDOMRe.renderToElementWithClassName(<Top />, "init-board");
