import { FoliageModel } from './model'

function App() {
  const prop = FoliageModel.exampleProp()

  return (
    <div>
      <h1>Foliage</h1>
      <p>Proposition from OCaml: <code>{prop}</code></p>
    </div>
  )
}

export default App
