# revelte

`$state()` and `$effect()` for react, WIP

```jsx
import type {} from 'revelte'

function App() {
  let count = $state(0)
  return <div onClick={() => count += 1}>{count}</div>
}
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
