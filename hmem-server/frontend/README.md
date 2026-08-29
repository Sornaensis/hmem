# Frontend validation

Use Node.js 20 LTS.

For a clean browser-test installation, run:

```sh
npm ci
npm run test:browser:install
npm test
```

`test:browser:install` provisions the exact Chromium revision required by the locked Playwright version. The timeline browser test starts from a new, empty browser context and runs the compiled production Elm fixture without authentication or backend dependencies.
