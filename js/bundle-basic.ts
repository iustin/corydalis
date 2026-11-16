// PageBasic bundle
/* eslint-disable @typescript-eslint/no-unused-vars */
import $ from 'jquery';
import 'bootstrap';
import { setupCsrf } from './csrf';
import { setupFlagActions } from './flag-actions';
import { notify } from './notify';

setupCsrf($);
setupFlagActions($, notify);
