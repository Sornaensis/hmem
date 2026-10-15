import { installTimestampDisplay } from './timestamp-display.js'
import { Elm } from './TimelineFixture.elm'

const app = Elm.TimelineFixture.init({ node: document.getElementById('app'), flags: null })

installTimestampDisplay(app)
