import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  plugins: [elmPlugin()],
  build: {
    outDir: '../static',
    emptyOutDir: true,
    rollupOptions: {
      output: {
        manualChunks(id) {
          const normalizedId = id.replace(/\\/g, '/')

          if (normalizedId.includes('/node_modules/cytoscape/')) {
            return 'cytoscape'
          }

          if (normalizedId.includes('/node_modules/')) {
            return 'vendor'
          }

          return undefined
        },
      },
    },
  },
  server: {
    port: 3000,
    proxy: {
      '/api/v1/ws': {
        target: 'ws://localhost:8420',
        ws: true,
      },
      '/api': {
        target: 'http://localhost:8420',
        changeOrigin: true,
      },
    },
  },
})
