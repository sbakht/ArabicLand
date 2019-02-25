import 'purecss/build/pure.css';
import './main.css';
import './_chips.scss';
import { Elm } from './App.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.App.init({
  node: document.getElementById('root'),
  flags: {ch: 'ch1',
    data: [
        [
            {
                word: 'We',
                answer: 'Ism'
            },
            {
                word: 'invited',
                answer: 'Fil'
            },
            {
                word: 'guests',
                answer: 'Ism'
            },
            {
                word: 'for',
                answer: 'Harf'
            },
            {
                word: 'dinner.',
                answer: 'Ism'
            },
        ],
        [
            {
                word: 'They',
                answer: 'Ism'
            },
            {
                word: 'arrived',
                answer: 'Fil'
            },
            {
                word: 'early.',
                answer: 'Ism'
            },
        ],
        [
            {
                word: 'I',
                answer: 'Ism'
            },
            {
                word: 'told',
                answer: 'Fil'
            },
            {
                word: 'my',
                answer: 'Ism'
            },
        ]
    ]
  }
});

registerServiceWorker();
