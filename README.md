# revelte

`$state()` and `$effect()` for react, WIP

```jsx
function App() {
  let count = $state(0)
  const inc = () => count += 1
  return <div onClick={inc}>{count}</div>
}
```

## Install

```sh
npm create vite@latest my-react-app -- --template react-swc-ts
cd my-react-app
npm i -D revelte
```

in `vite.config.ts` add `revelte` as a plugin:

```js
export default defineConfig({
  plugins: [react({plugins: [['revelte']]})],
})
```

```sh
npm run dev
```