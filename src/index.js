import './main.css';
import 'purecss/build/pure.css';
import { Elm } from './App.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.App.init({
  node: document.getElementById('root'),
  flags: "ch1"
});

registerServiceWorker();
