import 'purecss/build/pure.css';
import './main.css';
import './_chips.scss';
import { Elm } from './App.elm';
import registerServiceWorker from './registerServiceWorker';

//The teacher entered the classroom. His student was sleeping soundly. He threw a pencil, and the
  //student woke up suddenly. The student’s mother called the teacher the next day and confronted him
 // angrily. The teacher lost his job.

window.data = [
    [
        {
          word : 'The',
        },
        {
          word : 'teacher',
          answer: 'R'
        },
        {
          word : 'entered the',
        },
        {
          word : 'classroom',
          answer: 'J'
        },
    ],
    [
        {
          word : 'His',
          answer: 'J'
        },
        {
          word : 'student',
          answer: 'R'
        },
        {
          word : 'was sleeping',
        },
        {
          word : 'soundly',
          answer: 'N'
        },
    ],
    [
        {
          word : 'He',
          answer: 'R'
        },
        {
          word : 'threw a',
        },
        {
          word : 'pencil,',
          answer: 'N'
        },
        {
          word : 'and the',
        },
        {
          word : 'student',
          answer: 'R'
        },
        {
          word : 'woke up',
        },
        {
          word : 'suddenly',
          answer: 'N'
        },
    ],
    [
        {
          word : 'The',
        },
        {
          word : "student's",
          answer: 'J'
        },
        {
          word : 'mother',
          answer: 'R'
        },
        {
          word : 'called the',
        },
        {
          word : 'teacher',
          answer: 'N'
        },
        {
          word : 'the',
        },
        {
          word : 'next',
          answer: 'J'
        },
        {
          word : 'day',
          answer: 'N'
        },
        {
          word : 'and confronted',
        },
        {
          word : 'him',
          answer: 'N'
        },
        {
          word : 'angrily',
          answer: 'N'
        },
    ],
    [
        {
          word : 'The',
        },
        {
          word : 'teacher',
          answer: 'R'
        },
        {
          word : 'lost',
        },
        {
          word : 'his',
          answer: 'J'
        },
        {
          word : 'job',
          answer: 'N'
        },
    ],
];

window.data = [
    {
        text: 'bob',
        questions: [
            {
                type: 'radio',
                answer: 'R'
            },
            {
                type: 'radio',
                answer: 'H'
            }
        ]
    }
];

Elm.App.init({
  node: document.getElementById('root'),
  flags: {ch: 'ch2',
    data: window.data,
  }
});

registerServiceWorker();
