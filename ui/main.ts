// /* Redirect to mobile site.
//  As of early 2012, the fanciest mobile device is the "new iPad" with
//  a resolution of 2048x1536. However, Safari on that device reports
//  a resolution of 1024x768 on purpose, so we're safe for now. Note that
//  screen real estate is not the only reason to switch to the mobile site;
//  the MockTerminal implementation puts every character on a setTimeout
//  to achieve a "old, high-latency terminal" effect, and this really slows
//  down mobile devices, even iPads.
//
//  We also redirect IE<9 to the mobile site, since I haven't yet made the
//  terminal UI compatible with them. */
// if ((window.screen && window.screen.width < 800)
//     || window.attachEvent)
//     location.replace('//m.gay-lisp.org');
//
// document.addEventListener
//     ? document.addEventListener('DOMContentLoaded', main, false)
//     : window.attachEvent('onload', main);

import {MockTerminal} from "./mockterm";
import {RotaryNav} from "./rotary_nav";
import {TextResizer} from "./text_resizer";
import {VisibilityManager} from "./visibility_manager";

export function main() {
  asyncLoadSpec();
  setupTerminal();
  setupColors();
}

function asyncLoadSpec() {
  const req = new XMLHttpRequest();
  req.open('GET', 'r5rs.xhtml');
  req.onreadystatechange = () => {
    if (req.readyState === 4 && req.status === 200) {
      document.body.appendChild(req.responseXML!.getElementById('spec')!);
      setupSpecExamples();
      setupBackLinks();
      setupHeroText();
      setupNav();
      // To get the nav and hero text looking good
      manualResize();
    }
  };
  req.send();
}

function setupTerminal() {
  const textArea = document.getElementById('play')! as HTMLTextAreaElement;
  // The CSS assigns padding-bottom: 20% to the textarea, so there is always a nice buffer between
  // the command line and the bottom of the screen. Unfortunately, as of early 2012 Firefox applies
  // padding to textareas incorrectly, on the outside instead of the inside. The result is the
  // terminal scrolls below the bottom of the screen and is unusable. This bug
  // (https://bugzilla.mozilla.org/show_bug.cgi?id=157846) has been open since 2002. We sniff the
  // userAgent string and get rid of the padding if we're on Firefox.
  if (navigator && navigator.userAgent.search(/Firefox/) !== -1) {
    textArea.style.paddingBottom = '0';
    textArea.style.height = '100%';
  }
  new MockTerminal(textArea, 80, 5, 500)
      // .println(GayLisp.getMetadata().banner)
      .println(';; Type (tutorial) (with the parentheses) and press enter for an interactive tutorial.')
      .setPrompt('>> ')
      // .pushInterpreter((string: string, terminal: any /* TODO */ ) =>
      //     GayLisp.repl(string, (sideEffect: string) =>terminal.println(sideEffect)))
      // .pushInterpreter(tutorial)
      // .setInputCompleteHandler(GayLisp.willParse)
      .start();
}

function setupNav() {
  let startOn;
  switch (document.location!.hash) {
      // No fragment: start at the REPL
    case '':
      startOn = '#play';
      break;
      // Fragment corresponding to one of the rotary nav items: start at that item
    case '#play':
    case '#spec':
    case '#about':
      startOn = document.location!.hash;
      break;
      // Other fragment: it's inside the spec, start there.
    default:
      startOn = '#spec';
      break;
  }
  new VisibilityManager()
      .registerAnchors(document.querySelectorAll('.vis-manager'))
      .bringToFront(document.querySelector(startOn)!);

  // Set up the rotary-phone-like nav thing in the corner
  const rotaryNav = new RotaryNav(document.getElementById('logo')!, 40, 45, -45)
      .setTransitionSpeed(0.5)
      .setSelectClass('selected');

  const lis = document.getElementById('navlist')!.children;
  for (let i = 0; i < lis.length; ++i) {
    rotaryNav.push(lis[i] as HTMLElement, lis[i].getElementsByTagName('a')[0].hash);
  }

  rotaryNav.rotateToFront(startOn);
}

function setupBackLinks() {
  const h1s = document.querySelectorAll('#spec section > h1');

  // Decorate the major headings with links back to the top
  for (let i = 0; i < h1s.length; ++i) {
    const a = document.createElement('a');
    a.href = '#contents';
    a.appendChild(document.createTextNode('⬆'));
    h1s[i].parentElement!.insertBefore(a, h1s[i].nextElementSibling);
  }
}

function setupColors() {

  // Make the logo in the rotary nav thing change colors intermittently.
  const colors = [
    'rgba(255,0,0,0.8)', // red
    'rgba(0,0,0,0)',
    'rgba(255,165,0,0.8)', // orange
    'rgba(0,0,0,0)',
    'rgba(255,255,0,0.8)', // yellow
    'rgba(0,0,0,0)',
    'rgba(0,255,0,0.8)', // green
    'rgba(0,0,0,0)',
    'rgba(0,0,255,0.8)', // blue
    'rgba(0,0,0,0)',
    'rgba(75,0,130,0.8)', // indigo
    'rgba(0,0,0,0)',
    'rgba(238,130,238,0.8)', // violet
    'rgba(0,0,0,0)'];

  const changeColor = (i: number) => {
    document.getElementById('logo')!.style.backgroundColor = colors[i];
    setTimeout(changeColor, 10000, (i + 1) % colors.length);
  };

  setTimeout(changeColor, 10000, 0);
}

function setupHeroText() {
  const heroes = document.getElementsByClassName('hero');
  for (let i = 0; i < heroes.length; ++i) {
    new TextResizer(heroes[i] as HTMLElement);
  }
}

function setupSpecExamples() {

  // Replace the answers in the spec with buttons to call the interpreter
  const qs = document.querySelectorAll('.ex dt');

  for (let i = 0; i < qs.length; ++i) {
    const question = qs[i];
    const input = question.textContent!;
    const answer = question.nextElementSibling!; // <dd>
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'evalButton';
    button.setAttribute('data-input', input);

    const cb = (e: Event) => {
      const targetElement = e.target as Element;
      let output;
      try {
        output = ""; // TODO re-enable GayLisp.repl(targetElement.getAttribute('data-input'));
      } catch (x) {
        output = x.toString();
      }

      targetElement.parentElement!.replaceChild(
          document.createTextNode('⇒ ' + output),
          targetElement);
    };
    button.addEventListener('click', cb, false);
    button.appendChild(document.createTextNode('eval'));
    answer.replaceChild(button, answer.firstChild!);
  }
}

function manualResize() {
  const e = document.createEvent('Event');
  e.initEvent('resize', false, false);
  dispatchEvent(e);
}
