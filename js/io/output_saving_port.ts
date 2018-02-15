import {OutputPort} from './output_port';

export class OutputSavingPort extends OutputPort {
 constructor() {
   super();
 }

 dequeueOutput(): string|null {
     return null;
 }
}


