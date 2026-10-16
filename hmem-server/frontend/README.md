# hmem web interface

The Elm web interface manages observations, projects and tasks, with search,
history and a workspace lifecycle graph.

Use Node.js LTS (20 or later). From `hmem-server/frontend`:

```sh
npm ci
npm run test:browser:install
npm test
npm run dev
```

Vite serves the app on port 3000 and connects to hmem-server on port 8420.
For production assets, run `npm run build` from this directory.

See the [web interface guide](guide.md) for product use and the
[development reference](development.md) for configuration, tests and implementation
contracts. [Performance qualification](perf/performance.md) has separate commands
and evidence requirements.
