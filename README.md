# Before first run

1. Install elm:
   ```
   npm install -g elm
   ```
2. Install packages from `elm.json`:
   ```
   elm install
   ```
3. Install elm formatting:
    ```
    npm install -g elm-format
    ```

# How to run

Exposes app to [localhost:8000](http://localhost:8000):
```
elm reactor
```

**~ or ~**

Compiles `Main.elm` to `app.js` file, which is used in `index.html`:
```
elm make src/Main.elm --output app.js
```

# Formatting

```
elm-format .
```

# TODO

Nice-to-have:

- Labels on edges.
  - **actual state:** Labels are saved in nodes (type `Node`), but they are not rendered in diagram.
- Better dragging logic.
  - **actual state:** Mouse is going faster than dragged object. Because mouse position is from whole view, but nodes are rendered and moved only in half of the view (svg part).
- Dynamic node width based on text content.
  - **actual state:** Node width is defined as specific number.
- Enable *tab* key inside text area.
  - **actual state:** Pressing *tab* inside textarea will take focus out of textarea.