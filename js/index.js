import * as webnative from "webnative"
import * as webnativeElm from "webnative-elm"
import { Elm } from "/src/Main.elm"

const app = Elm.Main.init({});

webnative.setup.debug({enabled:true});

webnativeElm.setup({app, webnative});