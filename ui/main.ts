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

import {boot} from "../eval/boot";
import {isLineComplete} from "../repl/replutil";
import {SchemeSources} from "../scm/scheme_sources";
import {MockTerminal} from "./mockterm";
import {RotaryNav} from "./rotary_nav";
import {TextResizer} from "./text_resizer";
import {VisibilityManager} from "./visibility_manager";

document.addEventListener('DOMContentLoaded', main, false);

function main() {
  setupNav();
  manualResize();  // get the nav and hero text looking good
  setupTerminal();
  setupColors();
}

function setupTerminal() {
  const textArea = document.getElementById('play')! as HTMLTextAreaElement;
  const sources = new SchemeSources();
  const evaluator = boot(sources.syntax, sources.procedures);
  new MockTerminal(textArea, 80, 5, 500)
      .println(";; r5js") // TODO display banner
      .setPrompt('>> ')
      .pushInterpreter((string: string, terminal: MockTerminal) => evaluator.evaluate(string))
      .setInputCompleteHandler(isLineComplete)
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
  for (const li of lis) {
    rotaryNav.push(li as HTMLElement, li.getElementsByTagName('a')[0].hash);
  }

  rotaryNav.rotateToFront(startOn);
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

function manualResize() {
  const e = document.createEvent('Event');
  e.initEvent('resize', false, false);
  dispatchEvent(e);
}
