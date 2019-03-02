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
                answer: 'I'
            },
            {
                word: 'invited',
                answer: 'F'
            },
            {
                word: 'guests',
                answer: 'I'
            },
            {
                word: 'for',
            },
            {
                word: 'dinner.',
                answer: 'I'
            },
        ],
        [
            {
                word: 'They',
                answer: 'I'
            },
            {
                word: 'arrived',
                answer: 'F'
            },
            {
                word: 'early.',
                answer: 'I'
            },
        ],
        [
            {
                word: 'I',
                answer: 'I'
            },
            {
                word: 'told',
                answer: 'F'
            },
            {
                word: 'my',
                answer: 'I'
            },
            {
                word: 'son',
                answer: 'I'
            },
            {
                word: 'to',
                answer: 'H'
            },
            {
                word: 'give',
                answer: 'F'
            },
            {
                word: 'them',
                answer: 'I'
            },
            {
                word: 'fruits',
                answer: 'I'
            },
            {
                word: 'and',
                answer: 'H'
            },
            {
                word: 'drinks',
                answer: 'I'
            },
            {
                word: 'and',
                answer: 'H'
            },
            {
                word: 'I',
                answer: 'I'
            },
            {
                word: 'put',
                answer: 'F'
            },
            {
                word: 'the',
                answer: 'H'
            },
            {
                word: 'chicken',
                answer: 'I'
            },
            {
                word: 'in',
                answer: 'H'
            },
            {
                word: 'the',
                answer: 'H'
            },
            {
                word: 'oven',
                answer: 'I'
            },
            {
                word: 'hurriedly',
                answer: 'I'
            },
        ],
        [
            {
                word: 'He',
                answer: 'I'
            },
            {
                word: 'dropped',
                answer: 'F'
            },
            {
                word: 'the',
                answer: 'H'
            },
            {
                word: 'tray',
                answer: 'I'
            },
            {
                word: 'on',
                answer: 'H'
            },
            {
                word: 'the',
                answer: 'H'
            },
            {
                word: 'white',
                answer: 'I'
            },
            {
                word: 'carpet',
                answer: 'I'
            },
            {
                word: 'and',
                answer: 'H'
            },
            {
                word: 'the',
                answer: 'H'
            },
            {
                word: 'drinks',
                answer: 'I'
            },
            {
                word: 'spilled',
                answer: 'F'
            },
        ],
        [
            {
                word: 'Guests',
                answer: 'I'
            },
            {
                word: 'are',
                answer: 'H'
            },
            {
                word: 'coming',
                answer: 'F'
            },
            {
                word: 'again',
                answer: 'I'
            },
            {
                word: 'today',
                answer: 'I'
            },
        ],
        [
            {
                word: 'I',
                answer: 'I'
            },
            {
                word: 'will remind',
                answer: 'F'
            },
            {
                word: 'him',
                answer: 'I'
            },
            {
                word: 'to',
                answer: 'H'
            },
            {
                word: 'carry',
                answer: 'F'
            },
            {
                word: 'the',
                answer: 'H'
            },
            {
                word: 'tray',
                answer: 'I'
            },
            {
                word: 'carefully',
                answer: 'I'
            },
            {
                word: 'this',
                answer: 'I'
            },
            {
                word: 'time',
                answer: 'I'
            },
        ]
    ]
  }
});

registerServiceWorker();
