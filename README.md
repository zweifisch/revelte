# revelte

`$state()` and `$effect()` for react

```jsx
import type {} from 'revelte'

function App() {
  let count = $state(0)
  $effect(() => {
    console.log(`count: ${count}`)
  })
  return <div onClick={() => count += 1}>{count}</div>
}
```
## Why

You get immutable state for free:

```jsx
state.foo.bar += 1
```

instead of 

```jsx
setState({...state, foo: {...state.foo, bar: state.foo.bar + 1}})
```

## Setup

```sh
npm create vite@latest my-react-app -- --template react-swc-ts
cd my-react-app
npm i -D revelte
```

in `vite.config.ts` add `revelte` as a plugin:

```js
export default defineConfig({
  plugins: [react({plugins: [['revelte', {}]]})],
})
```

```sh
npm run dev
```

## When will state be updated

- reassigning $state variables, like `count = newVal`
- mutating object $state variables, like `foo.bar = newVal`
- mutating arrays, including `push`, `pop`, `unshift`, `shift`, `splice`, `fill`, `reverse`, `sort` and `copyWithin`
- always operate directly on the original $state object, otherwise state won't change

Instead of:
```js
let state = $state({count: 1})
const {count} = state
count += 1
```

Use:
```js
state.count += 1
```

## How does it work

Code are rewritten to use `useState` and `useEffect`, so there is almost no runtime overhead.
