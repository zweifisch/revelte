declare global {
  function $effect(fn: Function): Function | void;
  function $state<T>(x?: T): T;
}

export {}
