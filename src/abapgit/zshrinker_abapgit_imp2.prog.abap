********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.closeActivatedDropdown = function() {' ).
lo_buf->add( '  if (!this.activatedDropdown) return;' ).
lo_buf->add( '  this.activatedDropdown.classList.remove("force-nav-hover");' ).
lo_buf->add( '  this.activatedDropdown = null;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.displayHints = function(isActivate) {' ).
lo_buf->add( '  this.areHintsDisplayed = isActivate;' ).
lo_buf->add( '  for (var i = this.hintsMap.first; i <= this.hintsMap.last; i++) {' ).
lo_buf->add( '    var hint = this.hintsMap[i];' ).
lo_buf->add( '    if (isActivate) {' ).
lo_buf->add( '      hint.container.classList.remove("nodisplay");' ).
lo_buf->add( '      hint.pendingSpan.innerText   = "";' ).
lo_buf->add( '      hint.remainingSpan.innerText = hint.code;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      hint.container.classList.add("nodisplay");' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.hintActivate = function(hint) {' ).
lo_buf->add( '  if (hint.parent.nodeName === "A"' ).
lo_buf->add( '    // hint.parent.href doesn`t have a # at the end while accessing dropdowns the first time.' ).
lo_buf->add( '    // Seems like a idiosyncrasy of SAPGUI`s IE. So let`s ignore the last character.' ).
lo_buf->add( '    && (hint.parent.href.substr(0, hint.parent.href.length - 1) === document.location.href)// href is #' ).
lo_buf->add( '    && !hint.parent.onclick // no handler' ).
lo_buf->add( '    && hint.parent.parentElement && hint.parent.parentElement.nodeName === "LI") {' ).
lo_buf->add( '    // probably it is a dropdown ...' ).
lo_buf->add( '    this.activatedDropdown = hint.parent.parentElement;' ).
lo_buf->add( '    this.activatedDropdown.classList.toggle("force-nav-hover");' ).
lo_buf->add( '    hint.parent.focus();' ).
lo_buf->add( '  } else if (hint.parent.type === "checkbox") {' ).
lo_buf->add( '    this.toggleCheckbox(hint);' ).
lo_buf->add( '  } else if (hint.parent.type === "radio") {' ).
lo_buf->add( '    this.toggleRadioButton(hint);' ).
lo_buf->add( '  } else if (hint.parent.type === "submit") {' ).
lo_buf->add( '    hint.parent.click();' ).
lo_buf->add( '  } else if (hint.parent.nodeName === "INPUT" || hint.parent.nodeName === "TEXTAREA") {' ).
lo_buf->add( '    hint.parent.focus();' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    hint.parent.click();' ).
lo_buf->add( '    if (this.activatedDropdown) this.closeActivatedDropdown();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.toggleCheckbox = function(hint) {' ).
lo_buf->add( '  var checked = hint.parent.checked;' ).
lo_buf->add( '  this.triggerClickHandler(hint.parent.parentElement);' ).
lo_buf->add( '  if (checked === hint.parent.checked) {' ).
lo_buf->add( '    // fallback if no handler is registered' ).
lo_buf->add( '    hint.parent.checked = !hint.parent.checked;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.toggleRadioButton = function(hint) {' ).
lo_buf->add( '  this.triggerClickHandler(hint.parent);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.triggerClickHandler = function(el) {' ).
lo_buf->add( '  // ensures that onclick handler is executed' ).
lo_buf->add( '  // https://stackoverflow.com/questions/41981509/trigger-an-event-when-a-checkbox-is-changed-programmatically-via-javascript' ).
lo_buf->add( '  var event = document.createEvent("HTMLEvents");' ).
lo_buf->add( '  event.initEvent("click", false, true);' ).
lo_buf->add( '  el.dispatchEvent(event);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'LinkHints.prototype.filterHints = function() {' ).
lo_buf->add( '  var visibleHints = 0;' ).
lo_buf->add( '  for (var i = this.hintsMap.first; i <= this.hintsMap.last; i++) {' ).
lo_buf->add( '    var hint = this.hintsMap[i];' ).
lo_buf->add( '    if (i.toString().startsWith(this.pendingPath)) {' ).
lo_buf->add( '      hint.pendingSpan.innerText   = this.pendingPath;' ).
lo_buf->add( '      hint.remainingSpan.innerText = hint.code.substring(this.pendingPath.length);' ).
lo_buf->add( '      // hint.container.classList.remove("nodisplay"); // for backspace' ).
lo_buf->add( '      visibleHints++;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      hint.container.classList.add("nodisplay");' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '  return visibleHints;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'function activateLinkHints(linkHintHotKey) {' ).
lo_buf->add( '  if (!linkHintHotKey) return;' ).
lo_buf->add( '  var oLinkHint = new LinkHints(linkHintHotKey);' ).
lo_buf->add( '  document.addEventListener("keypress", oLinkHint.getHandler());' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Hotkeys' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function Hotkeys(oKeyMap) {' ).
lo_buf->add( '  this.oKeyMap = oKeyMap || {};' ).
lo_buf->add( '' ).
lo_buf->add( '  // these are the hotkeys provided by the backend' ).
lo_buf->add( '  Object.keys(this.oKeyMap).forEach(function(sKey) {' ).
lo_buf->add( '' ).
lo_buf->add( '    var action = this.oKeyMap[sKey];' ).
lo_buf->add( '' ).
lo_buf->add( '    // add a tooltip/title with the hotkey, currently only sapevents are supported' ).
lo_buf->add( '    this.getAllSapEventsForSapEventName(action).forEach(function(elAnchor) {' ).
lo_buf->add( '      elAnchor.title = elAnchor.title + " [" + sKey + "]";' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '    // We replace the actions with callback functions to unify' ).
lo_buf->add( '    // the hotkey execution' ).
lo_buf->add( '    this.oKeyMap[sKey] = function(oEvent) {' ).
lo_buf->add( '' ).
lo_buf->add( '      // gHelper is only valid for diff page' ).
lo_buf->add( '      var diffHelper = (window.gHelper || {});' ).
lo_buf->add( '' ).
lo_buf->add( '      // We have either a js function on this' ).
lo_buf->add( '      if (this[action]) {' ).
lo_buf->add( '        this[action].call(this);' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a method of the helper object for the diff page' ).
lo_buf->add( '      if (diffHelper[action]) {' ).
lo_buf->add( '        diffHelper[action].call(diffHelper);' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a global function' ).
lo_buf->add( '      if (window[action] && typeof (window[action]) === "function") {' ).
lo_buf->add( '        window[action].call(this);' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a SAP event link' ).
lo_buf->add( '      var sUiSapEventHref = this.getSapEventHref(action);' ).
lo_buf->add( '      if (sUiSapEventHref) {' ).
lo_buf->add( '        submitSapeventForm({}, sUiSapEventHref, "post");' ).
lo_buf->add( '        oEvent.preventDefault();' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a SAP event input' ).
lo_buf->add( '      var sUiSapEventInputAction = this.getSapEventInputAction(action);' ).
lo_buf->add( '      if (sUiSapEventInputAction) {' ).
lo_buf->add( '        submitSapeventForm({}, sUiSapEventInputAction, "post");' ).
lo_buf->add( '        oEvent.preventDefault();' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '      // Or a SAP event main form' ).
lo_buf->add( '      var elForm = this.getSapEventForm(action);' ).
lo_buf->add( '      if (elForm) {' ).
lo_buf->add( '        elForm.submit();' ).
lo_buf->add( '        oEvent.preventDefault();' ).
lo_buf->add( '        return;' ).
lo_buf->add( '      }' ).
lo_buf->add( '' ).
lo_buf->add( '    };' ).
lo_buf->add( '' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.showHotkeys = function() {' ).
lo_buf->add( '  var elHotkeys = document.querySelector("#hotkeys");' ).
lo_buf->add( '' ).
lo_buf->add( '  if (elHotkeys) {' ).
lo_buf->add( '    elHotkeys.style.display = (elHotkeys.style.display) ? "" : "none";' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getAllSapEventsForSapEventName = function (sSapEvent) {' ).
lo_buf->add( '  if (/^#+$/.test(sSapEvent)){' ).
lo_buf->add( '    // sSapEvent contains only #. Nothing sensible can be done here' ).
lo_buf->add( '    return [];' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var includesSapEvent = function(text){' ).
lo_buf->add( '    return (text.includes("sapevent") || text.includes("SAPEVENT"));' ).
lo_buf->add( '  };' ).
lo_buf->add( '' ).
lo_buf->add( '  return [].slice' ).
lo_buf->add( '    .call(document.querySelectorAll("a[href*="+ sSapEvent +"], input[formaction*="+ sSapEvent+"]"))' ).
lo_buf->add( '    .filter(function (elem) {' ).
lo_buf->add( '      return (elem.nodeName === "A" && includesSapEvent(elem.href)' ).
lo_buf->add( '          || (elem.nodeName === "INPUT" && includesSapEvent(elem.formAction)));' ).
lo_buf->add( '    });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getSapEventHref = function(sSapEvent) {' ).
lo_buf->add( '  return this.getAllSapEventsForSapEventName(sSapEvent)' ).
lo_buf->add( '    .filter(function(el) {' ).
lo_buf->add( '      // only anchors' ).
lo_buf->add( '      return (!!el.href);' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .map(function(oSapEvent) {' ).
lo_buf->add( '      return oSapEvent.href;' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .filter(this.eliminateSapEventFalsePositives(sSapEvent))' ).
lo_buf->add( '    .pop();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getSapEventInputAction = function(sSapEvent) {' ).
lo_buf->add( '  return this.getAllSapEventsForSapEventName(sSapEvent)' ).
lo_buf->add( '    .filter(function(el) {' ).
lo_buf->add( '      // input forms' ).
lo_buf->add( '      return (el.type === "submit");' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .map(function(oSapEvent) {' ).
lo_buf->add( '      return oSapEvent.formAction;' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .filter(this.eliminateSapEventFalsePositives(sSapEvent))' ).
lo_buf->add( '    .pop();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.getSapEventForm = function(sSapEvent) {' ).
lo_buf->add( '  return this.getAllSapEventsForSapEventName(sSapEvent)' ).
lo_buf->add( '    .filter(function(el) {' ).
lo_buf->add( '      // forms' ).
lo_buf->add( '      var parentForm = el.parentNode.parentNode.parentNode;' ).
lo_buf->add( '      return (el.type === "submit" && parentForm.nodeName === "FORM");' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .map(function(oSapEvent) {' ).
lo_buf->add( '      return oSapEvent.parentNode.parentNode.parentNode;' ).
lo_buf->add( '    })' ).
lo_buf->add( '    .pop();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.eliminateSapEventFalsePositives = function(sapEvent) {' ).
lo_buf->add( '  return function(sapEventAttr) {' ).
lo_buf->add( '    return sapEventAttr.match(new RegExp("\\b" + sapEvent + "\\b"));' ).
lo_buf->add( '  };' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.prototype.onkeydown = function(oEvent) {' ).
lo_buf->add( '  if (oEvent.defaultPrevented) {' ).
lo_buf->add( '    return;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (!Hotkeys.isHotkeyCallPossible()) {' ).
lo_buf->add( '    return;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var' ).
lo_buf->add( '    sKey     = oEvent.key || String.fromCharCode(oEvent.keyCode),' ).
lo_buf->add( '    fnHotkey = this.oKeyMap[sKey];' ).
lo_buf->add( '' ).
lo_buf->add( '  if (fnHotkey) {' ).
lo_buf->add( '    fnHotkey.call(this, oEvent);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.isHotkeyCallPossible = function() {' ).
lo_buf->add( '  var activeElementType     = ((document.activeElement && document.activeElement.nodeName) || "");' ).
lo_buf->add( '  var activeElementReadOnly = ((document.activeElement && document.activeElement.readOnly) || false);' ).
lo_buf->add( '' ).
lo_buf->add( '  return (activeElementReadOnly || (activeElementType !== "INPUT" && activeElementType !== "TEXTAREA"));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Hotkeys.addHotkeyToHelpSheet = function(key, description) {' ).
lo_buf->add( '  var hotkeysUl = document.querySelector("#hotkeys ul.hotkeys");' ).
lo_buf->add( '  if (!hotkeysUl) return;' ).
lo_buf->add( '' ).
lo_buf->add( '  var li        = document.createElement("li");' ).
lo_buf->add( '  var spanId    = document.createElement("span");' ).
lo_buf->add( '  var spanDescr = document.createElement("span");' ).
lo_buf->add( '' ).
lo_buf->add( '  spanId.className    = "key-id";' ).
lo_buf->add( '  spanId.innerText    = key;' ).
lo_buf->add( '  spanDescr.className = "key-descr";' ).
lo_buf->add( '  spanDescr.innerText = description;' ).
lo_buf->add( '  li.appendChild(spanId);' ).
lo_buf->add( '  li.appendChild(spanDescr);' ).
lo_buf->add( '' ).
lo_buf->add( '  hotkeysUl.appendChild(li);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'function setKeyBindings(oKeyMap) {' ).
lo_buf->add( '  var oHotkeys = new Hotkeys(oKeyMap);' ).
lo_buf->add( '' ).
lo_buf->add( '  document.addEventListener("keypress", oHotkeys.onkeydown.bind(oHotkeys));' ).
lo_buf->add( '  setTimeout(function() {' ).
lo_buf->add( '    var div                     = document.getElementById("hotkeys-hint");' ).
lo_buf->add( '    if  (div) div.style.opacity = 0.2;' ).
lo_buf->add( '  }, 4900);' ).
lo_buf->add( '  setTimeout(function() { toggleDisplay("hotkeys-hint") }, 5000);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Patch Logic (git add -p)' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  We have three type of cascading checkboxes.' ).
lo_buf->add( '  Which means that by clicking a file or section checkbox all corresponding line checkboxes are checked.' ).
lo_buf->add( '' ).
lo_buf->add( '  The id of the checkbox indicates its semantics and its membership.' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  1) file links' ).
lo_buf->add( '' ).
lo_buf->add( '      example id of file link' ).
lo_buf->add( '' ).
lo_buf->add( '      patch_file_zcl_abapgit_user_exit.clas.abap' ).
lo_buf->add( '      \________/ \_____________________________/' ).
lo_buf->add( '          |                   |' ).
lo_buf->add( '          |                   |____ file name' ).
lo_buf->add( '          |' ).
lo_buf->add( '          |' ).
lo_buf->add( '          |' ).
lo_buf->add( '      constant prefix' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( 'function PatchFile(sId) {' ).
lo_buf->add( '  var oRegex = new RegExp("(" + this.ID + ")_(.*$)");' ).
lo_buf->add( '  var oMatch = sId.match(oRegex);' ).
lo_buf->add( '' ).
lo_buf->add( '  this.id        = sId;' ).
lo_buf->add( '  this.prefix    = oMatch[1];' ).
lo_buf->add( '  this.file_name = oMatch[2];' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'PatchFile.prototype.ID = "patch_file";' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  2) section links within a file' ).
lo_buf->add( '' ).
lo_buf->add( '      example id of section link' ).
lo_buf->add( '' ).
lo_buf->add( '      patch_section_zcl_abapgit_user_exit.clas.abap_1' ).
lo_buf->add( '      \___________/ \_____________________________/ ^' ).
lo_buf->add( '            |                   |                   |' ).
lo_buf->add( '            |               file name               |' ).
lo_buf->add( '            |                                       |' ).
lo_buf->add( '            |                                       ------ section' ).
lo_buf->add( '            |' ).
lo_buf->add( '      constant prefix' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( 'function PatchSection(sId) {' ).
lo_buf->add( '  var oRegex = new RegExp("(" + this.ID + ")_(.*)_(\\d+$)");' ).
lo_buf->add( '  var oMatch = sId.match(oRegex);' ).
lo_buf->add( '' ).
lo_buf->add( '  this.id        = sId;' ).
lo_buf->add( '  this.prefix    = oMatch[1];' ).
lo_buf->add( '  this.file_name = oMatch[2];' ).
lo_buf->add( '  this.section   = oMatch[3];' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'PatchSection.prototype.ID = "patch_section";' ).
lo_buf->add( '' ).
lo_buf->add( '/*' ).
lo_buf->add( '  3) line links within a section' ).
lo_buf->add( '' ).
lo_buf->add( '      example id of line link' ).
lo_buf->add( '' ).
lo_buf->add( '      patch_line_zcl_abapgit_user_exit.clas.abap_1_25' ).
lo_buf->add( '      \________/ \_____________________________/ ^  ^' ).
lo_buf->add( '            ^                  ^                 |  |' ).
lo_buf->add( '            |                  |                 |  ------- line number' ).
lo_buf->add( '            |               file name            |' ).
lo_buf->add( '            |                                 section' ).
lo_buf->add( '            |' ).
lo_buf->add( '            |' ).
lo_buf->add( '      constant prefix' ).
lo_buf->add( '*/' ).
lo_buf->add( '' ).
lo_buf->add( 'function PatchLine() { }' ).
lo_buf->add( '' ).
lo_buf->add( 'PatchLine.prototype.ID = "patch_line";' ).
lo_buf->add( '' ).
lo_buf->add( 'function Patch() { }' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.ID = {' ).
lo_buf->add( '  STAGE: "stage"' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.ACTION = {' ).
lo_buf->add( '  PATCH_STAGE  : "patch_stage",' ).
lo_buf->add( '  REFRESH_LOCAL: "refresh_local",' ).
lo_buf->add( '  REFRESH_ALL  : "refresh_all"' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.escape = function(sFileName) {' ).
lo_buf->add( '  return sFileName' ).
lo_buf->add( '    .replace(/\./g, "\\.")' ).
lo_buf->add( '    .replace(/#/g, "\\#");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.preparePatch = function() {' ).
lo_buf->add( '  this.registerClickHandlerForFiles();' ).
lo_buf->add( '  this.registerClickHandlerForSections();' ).
lo_buf->add( '  this.registerClickHandlerForLines();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.buildSelectorInputStartsWithId = function(sId) {' ).
lo_buf->add( '  return "input[id^=''" + sId + "'']";' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForFiles = function() {' ).
lo_buf->add( '  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchFile.prototype.ID), this.onClickFileCheckbox);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForSections = function() {' ).
lo_buf->add( '  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchSection.prototype.ID), this.onClickSectionCheckbox);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForLines = function() {' ).
lo_buf->add( '  this.registerClickHandlerForSelectorParent(this.buildSelectorInputStartsWithId(PatchLine.prototype.ID), this.onClickLineCheckbox);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerClickHandlerForSelectorParent = function(sSelector, fnCallback) {' ).
lo_buf->add( '  var elAll = document.querySelectorAll(sSelector);' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAll, function(elem) {' ).
lo_buf->add( '    elem.parentElement.addEventListener("click", fnCallback.bind(this));' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllLineCheckboxesForFile = function(oFile) {' ).
lo_buf->add( '  return this.getAllLineCheckboxesForId(oFile.id, PatchFile.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllSectionCheckboxesForFile = function(oFile) {' ).
lo_buf->add( '  return this.getAllSectionCheckboxesForId(oFile.id, PatchFile.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllLineCheckboxesForSection = function(oSection) {' ).
lo_buf->add( '  return this.getAllLineCheckboxesForId(oSection.id, PatchSection.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllLineCheckboxesForId = function(sId, sIdPrefix) {' ).
lo_buf->add( '  return this.getAllCheckboxesForId(sId, sIdPrefix, PatchLine.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllSectionCheckboxesForId = function(sId, sIdPrefix) {' ).
lo_buf->add( '  return this.getAllCheckboxesForId(sId, sIdPrefix, PatchSection.prototype.ID);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getAllCheckboxesForId = function(sId, sIdPrefix, sNewIdPrefix) {' ).
lo_buf->add( '  var oRegex = new RegExp("^" + sIdPrefix);' ).
lo_buf->add( '' ).
lo_buf->add( '  sId = sId.replace(oRegex, sNewIdPrefix);' ).
lo_buf->add( '  return document.querySelectorAll(this.buildSelectorInputStartsWithId(this.escape(sId)));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.getToggledCheckbox = function(oEvent) {' ).
lo_buf->add( '  var elCheckbox = null;' ).
lo_buf->add( '' ).
lo_buf->add( '  // We have either an input element or any element with input child' ).
lo_buf->add( '  // in the latter case we have to toggle the checkbox manually' ).
lo_buf->add( '  if (oEvent.srcElement.nodeName === "INPUT") {' ).
lo_buf->add( '    elCheckbox = oEvent.srcElement;' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    elCheckbox = this.toggleCheckbox(oEvent.srcElement.querySelector("INPUT"));' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  return elCheckbox;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.toggleCheckbox = function(elCheckbox) {' ).
lo_buf->add( '  elCheckbox.checked = !elCheckbox.checked;' ).
lo_buf->add( '  return elCheckbox;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.onClickFileCheckbox = function(oEvent) {' ).
lo_buf->add( '  var elCheckbox                   = this.getToggledCheckbox(oEvent);' ).
lo_buf->add( '  var oFile                        = new PatchFile(elCheckbox.id);' ).
lo_buf->add( '  var elAllLineCheckboxesOfFile    = this.getAllLineCheckboxesForFile(oFile);' ).
lo_buf->add( '  var elAllSectionCheckboxesOfFile = this.getAllSectionCheckboxesForFile(oFile);' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAllLineCheckboxesOfFile, function(elem) {' ).
lo_buf->add( '    elem.checked = elCheckbox.checked;' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAllSectionCheckboxesOfFile, function(elem) {' ).
lo_buf->add( '    elem.checked = elCheckbox.checked;' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.onClickSectionCheckbox = function(oEvent) {' ).
lo_buf->add( '  var elSrcElement = this.getToggledCheckbox(oEvent);' ).
lo_buf->add( '  var oSection     = new PatchSection(elSrcElement.id);' ).
lo_buf->add( '  this.clickAllLineCheckboxesInSection(oSection, elSrcElement.checked);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.onClickLineCheckbox = function(oEvent) {' ).
lo_buf->add( '  this.getToggledCheckbox(oEvent);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.clickAllLineCheckboxesInSection = function(oSection, bChecked) {' ).
lo_buf->add( '  var elAllLineCheckboxesOfSection = this.getAllLineCheckboxesForSection(oSection);' ).
lo_buf->add( '' ).
lo_buf->add( '  [].forEach.call(elAllLineCheckboxesOfSection, function(elem) {' ).
lo_buf->add( '    elem.checked = bChecked;' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.registerStagePatch = function() {' ).
lo_buf->add( '  var elStage        = document.querySelector("#" + this.ID.STAGE);' ).
lo_buf->add( '  var REFRESH_PREFIX = "refresh";' ).
lo_buf->add( '' ).
lo_buf->add( '  elStage.addEventListener("click", this.submitPatch.bind(this, this.ACTION.PATCH_STAGE));' ).
lo_buf->add( '' ).
lo_buf->add( '  var aRefresh = document.querySelectorAll("[id*=" + REFRESH_PREFIX + "]");' ).
lo_buf->add( '  [].forEach.call(aRefresh, function(el) {' ).
lo_buf->add( '    el.addEventListener("click", memorizeScrollPosition(this.submitPatch.bind(this, el.id)).bind(this));' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '' ).
lo_buf->add( '  // for hotkeys' ).
lo_buf->add( '  window.stagePatch = function() {' ).
lo_buf->add( '    this.submitPatch(this.ACTION.PATCH_STAGE);' ).
lo_buf->add( '  }.bind(this);' ).
lo_buf->add( '' ).
lo_buf->add( '  window.refreshLocal = memorizeScrollPosition(function() {' ).
lo_buf->add( '    this.submitPatch(this.ACTION.REFRESH_LOCAL);' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '' ).
lo_buf->add( '  window.refreshAll = memorizeScrollPosition(function() {' ).
lo_buf->add( '    this.submitPatch(this.ACTION.REFRESH_ALL);' ).
lo_buf->add( '  }.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.submitPatch = function(action) {' ).
lo_buf->add( '  // Collect add and remove info and submit to backend' ).
lo_buf->add( '  var aAddPatch    = this.collectElementsForCheckboxId(PatchLine.prototype.ID, true);' ).
lo_buf->add( '  var aRemovePatch = this.collectElementsForCheckboxId(PatchLine.prototype.ID, false);' ).
lo_buf->add( '' ).
lo_buf->add( '  submitSapeventForm({ add: aAddPatch, remove: aRemovePatch }, action, "post");' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'Patch.prototype.collectElementsForCheckboxId = function(sId, bChecked) {' ).
lo_buf->add( '  var sSelector = this.buildSelectorInputStartsWithId(sId);' ).
lo_buf->add( '' ).
lo_buf->add( '  return [].slice.call(document.querySelectorAll(sSelector))' ).
lo_buf->add( '    .filter(function(elem) {' ).
lo_buf->add( '      return (elem.checked === bChecked);' ).
lo_buf->add( '    }).map(function(elem) {' ).
lo_buf->add( '      return elem.id;' ).
lo_buf->add( '    });' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'function preparePatch() {' ).
lo_buf->add( '  var oPatch = new Patch();' ).
lo_buf->add( '  oPatch.preparePatch();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function registerStagePatch() {' ).
lo_buf->add( '  var oPatch = new Patch();' ).
lo_buf->add( '  oPatch.registerStagePatch();' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Command Palette (Ctrl + P)' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// fuzzy match helper' ).
lo_buf->add( '// return non empty marked string in case it fits the filter' ).
lo_buf->add( '// abc + b = a<mark>b</mark>c' ).
lo_buf->add( 'function fuzzyMatchAndMark(str, filter) {' ).
lo_buf->add( '  var markedStr   = "";' ).
lo_buf->add( '  var filterLower = filter.toLowerCase();' ).
lo_buf->add( '  var strLower    = str.toLowerCase();' ).
lo_buf->add( '  var cur         = 0;' ).
lo_buf->add( '' ).
lo_buf->add( '  for (var i = 0; i < filter.length; i++) {' ).
lo_buf->add( '    while (filterLower[i] !== strLower[cur] && cur < str.length) {' ).
lo_buf->add( '      markedStr += str[cur++];' ).
lo_buf->add( '    }' ).
lo_buf->add( '    if (cur === str.length) break;' ).
lo_buf->add( '    markedStr += "<mark>" + str[cur++] + "</mark>";' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  var matched = i === filter.length;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (matched && cur < str.length) markedStr += str.substring(cur);' ).
lo_buf->add( '  return matched ? markedStr: null;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function CommandPalette(commandEnumerator, opts) {' ).
lo_buf->add( '  if (typeof commandEnumerator !== "function") throw Error("commandEnumerator must be a function");' ).
lo_buf->add( '  if (typeof opts !== "object") throw Error("opts must be an object");' ).
lo_buf->add( '  if (typeof opts.toggleKey !== "string" || !opts.toggleKey) throw Error("toggleKey must be a string");' ).
lo_buf->add( '  this.commands = commandEnumerator();' ).
lo_buf->add( '  if (!this.commands) return;' ).
lo_buf->add( '  // this.commands = [{' ).
lo_buf->add( '  //   action:    "sap_event_action_code_with_params"' ).
lo_buf->add( '  //   iconClass: "icon icon_x ..."' ).
lo_buf->add( '  //   title:     "my command X"' ).
lo_buf->add( '  // }, ...];' ).
lo_buf->add( '' ).
lo_buf->add( '  if (opts.toggleKey[0] === "^") {' ).
lo_buf->add( '    this.toggleKeyCtrl = true;' ).
lo_buf->add( '    this.toggleKey     = opts.toggleKey.substring(1);' ).
lo_buf->add( '    if (!this.toggleKey) throw Error("Incorrect toggleKey");' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    this.toggleKeyCtrl = false;' ).
lo_buf->add( '    this.toggleKey     = opts.toggleKey;' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.hotkeyDescription = opts.hotkeyDescription;' ).
lo_buf->add( '  this.elements          = {' ).
lo_buf->add( '    palette: null,' ).
lo_buf->add( '    ul     : null,' ).
lo_buf->add( '    input  : null' ).
lo_buf->add( '  };' ).
lo_buf->add( '  this.selectIndex = -1; // not selected' ).
lo_buf->add( '  this.filter      = "";' ).
lo_buf->add( '  this.renderAndBindElements();' ).
lo_buf->add( '  this.hookEvents();' ).
lo_buf->add( '  Hotkeys.addHotkeyToHelpSheet(opts.toggleKey, opts.hotkeyDescription);' ).
lo_buf->add( '' ).
lo_buf->add( '  if (!CommandPalette.instances) {' ).
lo_buf->add( '    CommandPalette.instances = [];' ).
lo_buf->add( '  }' ).
lo_buf->add( '  CommandPalette.instances.push(this);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.hookEvents = function() {' ).
lo_buf->add( '  document.addEventListener("keydown", this.handleToggleKey.bind(this));' ).
lo_buf->add( '  this.elements.input.addEventListener("keyup", this.handleInputKey.bind(this));' ).
lo_buf->add( '  this.elements.ul.addEventListener("click", this.handleUlClick.bind(this));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.renderCommandItem = function(cmd) {' ).
lo_buf->add( '  var li = document.createElement("li");' ).
lo_buf->add( '  if (cmd.iconClass) {' ).
lo_buf->add( '    var icon = document.createElement("i");' ).
lo_buf->add( '' ).
lo_buf->add( '    icon.className = cmd.iconClass;' ).
lo_buf->add( '    li.appendChild(icon);' ).
lo_buf->add( '  }' ).
lo_buf->add( '  var titleSpan = document.createElement("span");' ).
lo_buf->add( '  li.appendChild(titleSpan);' ).
lo_buf->add( '  cmd.element   = li;' ).
lo_buf->add( '  cmd.titleSpan = titleSpan;' ).
lo_buf->add( '  return li;' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.renderAndBindElements = function() {' ).
lo_buf->add( '  var div   = document.createElement("div");' ).
lo_buf->add( '  var input = document.createElement("input");' ).
lo_buf->add( '  var ul    = document.createElement("ul");' ).
lo_buf->add( '' ).
lo_buf->add( '  div.className     = "cmd-palette";' ).
lo_buf->add( '  div.style.display = "none";' ).
lo_buf->add( '  input.placeholder = this.hotkeyDescription;' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) ul.appendChild(this.renderCommandItem(this.commands[i]));' ).
lo_buf->add( '  div.appendChild(input);' ).
lo_buf->add( '  div.appendChild(ul);' ).
lo_buf->add( '' ).
lo_buf->add( '  this.elements.palette = div;' ).
lo_buf->add( '  this.elements.input   = input;' ).
lo_buf->add( '  this.elements.ul      = ul;' ).
lo_buf->add( '  document.body.appendChild(div);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.handleToggleKey = function(event) {' ).
lo_buf->add( '  if (event.key !== this.toggleKey) return;' ).
lo_buf->add( '  if (this.toggleKeyCtrl && !event.ctrlKey) return;' ).
lo_buf->add( '  this.toggleDisplay();' ).
lo_buf->add( '  event.preventDefault();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.handleInputKey = function(event) {' ).
lo_buf->add( '  if (event.key === "ArrowUp" || event.key === "Up") {' ).
lo_buf->add( '    this.selectPrev();' ).
lo_buf->add( '  } else if (event.key === "ArrowDown" || event.key === "Down") {' ).
lo_buf->add( '    this.selectNext();' ).
lo_buf->add( '  } else if (event.key === "Enter") {' ).
lo_buf->add( '    this.exec(this.getSelected());' ).
lo_buf->add( '  } else if (event.key === "Backspace" && !this.filter) {' ).
lo_buf->add( '    this.toggleDisplay(false);' ).
lo_buf->add( '  } else if (this.filter !== this.elements.input.value) {' ).
lo_buf->add( '    this.filter = this.elements.input.value;' ).
lo_buf->add( '    this.applyFilter();' ).
lo_buf->add( '    this.selectFirst();' ).
lo_buf->add( '  }' ).
lo_buf->add( '  event.preventDefault();' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.applyFilter = function() {' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) {' ).
lo_buf->add( '    var cmd = this.commands[i];' ).
lo_buf->add( '    if (!this.filter) {' ).
lo_buf->add( '      cmd.element.style.display = "";' ).
lo_buf->add( '      cmd.titleSpan.innerText   = cmd.title;' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      var matchedTitle = fuzzyMatchAndMark(cmd.title, this.filter);' ).
lo_buf->add( '      if (matchedTitle) {' ).
lo_buf->add( '        cmd.titleSpan.innerHTML   = matchedTitle;' ).
lo_buf->add( '        cmd.element.style.display = "";' ).
lo_buf->add( '      } else {' ).
lo_buf->add( '        cmd.element.style.display = "none";' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.applySelectIndex = function(newIndex) {' ).
lo_buf->add( '  if (newIndex !== this.selectIndex) {' ).
lo_buf->add( '    if (this.selectIndex >= 0) this.commands[this.selectIndex].element.classList.remove("selected");' ).
lo_buf->add( '    var newCmd = this.commands[newIndex];' ).
lo_buf->add( '    newCmd.element.classList.add("selected");' ).
lo_buf->add( '    this.selectIndex = newIndex;' ).
lo_buf->add( '    this.adjustScrollPosition(newCmd.element);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.selectFirst = function() {' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) {' ).
lo_buf->add( '    if (this.commands[i].element.style.display === "none") continue; // skip hidden' ).
lo_buf->add( '    this.applySelectIndex(i);' ).
lo_buf->add( '    break;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.selectNext = function() {' ).
lo_buf->add( '  for (var i = this.selectIndex + 1; i < this.commands.length; i++) {' ).
lo_buf->add( '    if (this.commands[i].element.style.display === "none") continue; // skip hidden' ).
lo_buf->add( '    this.applySelectIndex(i);' ).
lo_buf->add( '    break;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.selectPrev = function() {' ).
lo_buf->add( '  for (var i = this.selectIndex - 1; i >= 0; i--) {' ).
lo_buf->add( '    if (this.commands[i].element.style.display === "none") continue; // skip hidden' ).
lo_buf->add( '    this.applySelectIndex(i);' ).
lo_buf->add( '    break;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.getSelected = function() {' ).
lo_buf->add( '  return this.commands[this.selectIndex];' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.adjustScrollPosition = function(itemElement) {' ).
lo_buf->add( '  var bItem      = itemElement.getBoundingClientRect();' ).
lo_buf->add( '  var bContainer = this.elements.ul.getBoundingClientRect();' ).
lo_buf->add( '' ).
lo_buf->add( '  bItem.top         = Math.round(bItem.top);' ).
lo_buf->add( '  bItem.bottom      = Math.round(bItem.bottom);' ).
lo_buf->add( '  bItem.height      = Math.round(bItem.height);' ).
lo_buf->add( '  bItem.mid         = Math.round(bItem.top + bItem.height / 2);' ).
lo_buf->add( '  bContainer.top    = Math.round(bContainer.top);' ).
lo_buf->add( '  bContainer.bottom = Math.round(bContainer.bottom);' ).
lo_buf->add( '' ).
lo_buf->add( '  if (bItem.mid > bContainer.bottom - 2) {' ).
lo_buf->add( '    this.elements.ul.scrollTop += bItem.bottom - bContainer.bottom;' ).
lo_buf->add( '  } else if (bItem.mid < bContainer.top + 2) {' ).
lo_buf->add( '    this.elements.ul.scrollTop += bItem.top - bContainer.top;' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.toggleDisplay = function(forceState) {' ).
lo_buf->add( '  var isDisplayed   = (this.elements.palette.style.display !== "none");' ).
lo_buf->add( '  var tobeDisplayed = (forceState !== undefined) ? forceState : !isDisplayed;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (tobeDisplayed) {' ).
lo_buf->add( '    // auto close other command palettes' ).
lo_buf->add( '    CommandPalette.instances.forEach(function(instance) {' ).
lo_buf->add( '      instance.elements.palette.style.display = "none";' ).
lo_buf->add( '    });' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  this.elements.palette.style.display = tobeDisplayed ? "" : "none";' ).
lo_buf->add( '  if (tobeDisplayed) {' ).
lo_buf->add( '    this.elements.input.value = "";' ).
lo_buf->add( '    this.elements.input.focus();' ).
lo_buf->add( '    this.applyFilter();' ).
lo_buf->add( '    this.selectFirst();' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.getCommandByElement = function(element) {' ).
lo_buf->add( '  for (var i = 0; i < this.commands.length; i++) {' ).
lo_buf->add( '    if (this.commands[i].element === element) return this.commands[i];' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.handleUlClick = function(event) {' ).
lo_buf->add( '  var element = event.target || event.srcElement;' ).
lo_buf->add( '  if (!element) return;' ).
lo_buf->add( '  if (element.nodeName === "SPAN") element = element.parentNode;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (element.nodeName === "I") element = element.parentNode;' ).
lo_buf->add( '' ).
lo_buf->add( '  if (element.nodeName !== "LI") return;' ).
lo_buf->add( '  this.exec(this.getCommandByElement(element));' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( 'CommandPalette.prototype.exec = function(cmd) {' ).
lo_buf->add( '  if (!cmd) return;' ).
lo_buf->add( '  this.toggleDisplay(false);' ).
lo_buf->add( '  if (typeof cmd.action === "function") {' ).
lo_buf->add( '    cmd.action();' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    submitSapeventForm(null, cmd.action);' ).
lo_buf->add( '  }' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '// Is any command palette visible?' ).
lo_buf->add( 'CommandPalette.isVisible = function() {' ).
lo_buf->add( '  return CommandPalette.instances.reduce(function(result, instance) { return result || instance.elements.palette.style.display !== "none" }, false);' ).
lo_buf->add( '};' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Command Enumerators' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function createRepoCatalogEnumerator(catalog, action) {' ).
lo_buf->add( '  // expecting [{ key, isOffline, displayName }]' ).
lo_buf->add( '  return function() {' ).
lo_buf->add( '    return catalog.map(function(i) {' ).
lo_buf->add( '      return {' ).
lo_buf->add( '        action   : action + "?key=" + i.key,' ).
lo_buf->add( '        iconClass: i.isOffline ? "icon icon-plug darkgrey" : "icon icon-cloud-upload-alt blue",' ).
lo_buf->add( '        title    : i.displayName' ).
lo_buf->add( '      };' ).
lo_buf->add( '    });' ).
lo_buf->add( '  };' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function enumerateUiActions() {' ).
lo_buf->add( '  var items = [];' ).
lo_buf->add( '  function processUL(ulNode, prefix) {' ).
lo_buf->add( '    for (var i = 0; i < ulNode.children.length; i++) {' ).
lo_buf->add( '      var item = ulNode.children[i];' ).
lo_buf->add( '      if (item.nodeName !== "LI") continue; // unexpected node' ).
lo_buf->add( '      if (item.children.length >= 2 && item.children[1].nodeName === "UL") {' ).
lo_buf->add( '        // submenu detected' ).
lo_buf->add( '        var menutext = item.children[0].innerText;' ).
lo_buf->add( '        // special treatment for menus without text' ).
lo_buf->add( '        if (!menutext) {' ).
lo_buf->add( '          menutext = item.children[0].getAttribute("title");' ).
lo_buf->add( '        }' ).
lo_buf->add( '        processUL(item.children[1], menutext);' ).
lo_buf->add( '      } else if (item.firstElementChild && item.firstElementChild.nodeName === "A") {' ).
lo_buf->add( '        var anchor = item.firstElementChild;' ).
lo_buf->add( '        if (anchor.href && anchor.href !== "#") items.push([anchor, prefix]);' ).
lo_buf->add( '      }' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  // toolbars' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("[id*=toolbar]"))' ).
lo_buf->add( '    .filter(function(toolbar) {' ).
lo_buf->add( '      return (toolbar && toolbar.nodeName === "UL");' ).
lo_buf->add( '    }).forEach(function(toolbar) {' ).
lo_buf->add( '      processUL(toolbar);' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  items = items.map(function(item) {' ).
lo_buf->add( '    var action = "";' ).
lo_buf->add( '    var anchor = item[0];' ).
lo_buf->add( '    if (anchor.href.includes("#")) {' ).
lo_buf->add( '      action = function() {' ).
lo_buf->add( '        anchor.click();' ).
lo_buf->add( '      };' ).
lo_buf->add( '    } else {' ).
lo_buf->add( '      action = anchor.href.replace("sapevent:", "");' ).
lo_buf->add( '    }' ).
lo_buf->add( '    var prefix = item[1];' ).
lo_buf->add( '    return {' ).
lo_buf->add( '      action: action,' ).
lo_buf->add( '      title : (prefix ? prefix + ": " : "") + anchor.innerText.trim()' ).
lo_buf->add( '    };' ).
lo_buf->add( '  });' ).
lo_buf->add( '' ).
lo_buf->add( '  // forms' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("input[type=''submit'']"))' ).
lo_buf->add( '    .forEach(function(input) {' ).
lo_buf->add( '      items.push({' ).
lo_buf->add( '        action: function() {' ).
lo_buf->add( '          if (input.form.action.includes(input.formAction) || input.classList.contains("main")) {' ).
lo_buf->add( '            input.form.submit();' ).
lo_buf->add( '          } else {' ).
lo_buf->add( '            submitSapeventForm({}, input.formAction, "post", input.form);' ).
lo_buf->add( '          }' ).
lo_buf->add( '        },' ).
lo_buf->add( '        title: input.value + " " + input.title.replace(/\[.*\]/, "")' ).
lo_buf->add( '      });' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  // radio buttons' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("input[type=''radio'']"))' ).
lo_buf->add( '    .forEach(function(input) {' ).
lo_buf->add( '      items.push({' ).
lo_buf->add( '        action: function() {' ).
lo_buf->add( '          input.click();' ).
lo_buf->add( '        },' ).
lo_buf->add( '        title: document.querySelector("label[for=''" + input.id + "'']").textContent' ).
lo_buf->add( '      });' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  // others:' ).
lo_buf->add( '  // - links inside forms' ).
lo_buf->add( '  // - label links' ).
lo_buf->add( '  // - command links' ).
lo_buf->add( '  // - other header links' ).
lo_buf->add( '  [].slice.call(document.querySelectorAll("form a, a.command, #header ul:not([id*=''toolbar'']) a"))' ).
lo_buf->add( '    .filter(function(anchor) {' ).
lo_buf->add( '      return !!anchor.title || !!anchor.text;' ).
lo_buf->add( '    }).forEach(function(anchor) {' ).
lo_buf->add( '      items.push({' ).
lo_buf->add( '        action: function() {' ).
lo_buf->add( '          anchor.click();' ).
lo_buf->add( '        },' ).
lo_buf->add( '        title: (function() {' ).
lo_buf->add( '          var result = anchor.title + anchor.text;' ).
lo_buf->add( '          if (anchor.href.includes("label")) {' ).
lo_buf->add( '            result = "Label: " + result;' ).
lo_buf->add( '          }' ).
lo_buf->add( '          return result;' ).
lo_buf->add( '        })()' ).
lo_buf->add( '      });' ).
lo_buf->add( '    });' ).
lo_buf->add( '' ).
lo_buf->add( '  return items;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function enumerateJumpAllFiles() {' ).
lo_buf->add( '  var root = document.getElementById("jump");' ).
lo_buf->add( '  if (!root || root.nodeName !== "UL") return null;' ).
lo_buf->add( '' ).
lo_buf->add( '  return Array' ).
lo_buf->add( '    .prototype.slice.call(root.children)' ).
lo_buf->add( '    .filter(function(elem) { return elem.nodeName === "LI" })' ).
lo_buf->add( '    .map(function(listItem) {' ).
lo_buf->add( '      var title = listItem.children[0].childNodes[0].textContent;' ).
lo_buf->add( '      return {' ).
lo_buf->add( '        action: root.onclick.bind(null, title),' ).
lo_buf->add( '        title : title' ).
lo_buf->add( '      };' ).
lo_buf->add( '    });' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Save Scroll Position' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( 'function saveScrollPosition() {' ).
lo_buf->add( '  // Not supported by Java GUI' ).
lo_buf->add( '  try { if (!window.sessionStorage) { return } }' ).
lo_buf->add( '  catch (err) { return }' ).
lo_buf->add( '' ).
lo_buf->add( '  window.sessionStorage.setItem("scrollTop", document.querySelector("html").scrollTop);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function restoreScrollPosition() {' ).
lo_buf->add( '  // Not supported by Java GUI' ).
lo_buf->add( '  try { if (!window.sessionStorage) { return } }' ).
lo_buf->add( '  catch (err) { return }' ).
lo_buf->add( '' ).
lo_buf->add( '  var scrollTop = window.sessionStorage.getItem("scrollTop");' ).
lo_buf->add( '  if (scrollTop) {' ).
lo_buf->add( '    document.querySelector("html").scrollTop = scrollTop;' ).
lo_buf->add( '  }' ).
lo_buf->add( '  window.sessionStorage.setItem("scrollTop", 0);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( 'function memorizeScrollPosition(fn) {' ).
lo_buf->add( '  return function() {' ).
lo_buf->add( '    saveScrollPosition();' ).
lo_buf->add( '    return fn.call(this, fn.args);' ).
lo_buf->add( '  }.bind(this);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Sticky Header' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '/* https://www.w3schools.com/howto/howto_js_navbar_sticky.asp */' ).
lo_buf->add( '/* Note: We have to use JS since IE does not support CSS position:sticky */' ).
lo_buf->add( '' ).
lo_buf->add( '// When the user scrolls the page, execute toggleSticky' ).
lo_buf->add( 'window.onscroll = function() { toggleSticky() };' ).
lo_buf->add( '' ).
lo_buf->add( '// Add the sticky class to the navbar when you reach its scroll position.' ).
lo_buf->add( '// Remove "sticky" when you leave the scroll position' ).
lo_buf->add( 'function toggleSticky() {' ).
lo_buf->add( '  var body   = document.getElementsByTagName("body")[0];' ).
lo_buf->add( '  var header = document.getElementById("header");' ).
lo_buf->add( '  var sticky = header.offsetTop;' ).
lo_buf->add( '' ).
lo_buf->add( '  var stickyClass = "sticky";' ).
lo_buf->add( '  if (body.classList.contains("full_width")) {' ).
lo_buf->add( '    stickyClass = "sticky_full_width";' ).
lo_buf->add( '  }' ).
lo_buf->add( '' ).
lo_buf->add( '  if (window.pageYOffset >= sticky) {' ).
lo_buf->add( '    header.classList.add(stickyClass);' ).
lo_buf->add( '  } else {' ).
lo_buf->add( '    header.classList.remove(stickyClass);' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '/**********************************************************' ).
lo_buf->add( ' * Browser Control' ).
lo_buf->add( ' **********************************************************/' ).
lo_buf->add( '' ).
lo_buf->add( '// Toggle display of warning message when using Edge (based on Chromium) browser control' ).
lo_buf->add( '// Todo: Remove once https://github.com/abapGit/abapGit/issues/4841 is fixed' ).
lo_buf->add( 'function toggleBrowserControlWarning(){' ).
lo_buf->add( '  if (!navigator.userAgent.includes("Edg")){' ).
lo_buf->add( '    var elBrowserControlWarning = document.getElementById("browser-control-warning");' ).
lo_buf->add( '    if (elBrowserControlWarning) {' ).
lo_buf->add( '      elBrowserControlWarning.style.display = "none";' ).
lo_buf->add( '    }' ).
lo_buf->add( '  }' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '// Output type of HTML control in the abapGit footer' ).
lo_buf->add( 'function displayBrowserControlFooter() {' ).
lo_buf->add( '  var out = document.getElementById("browser-control-footer");' ).
lo_buf->add( '  out.innerHTML = " - " + ( navigator.userAgent.includes("Edg") ? "Edge" : "IE"  );' ).
lo_buf->add( '}' ).
    li_asset_man->register_asset(
      iv_url       = 'js/common.js'
      iv_type      = 'text/javascript'
      iv_mime_name = 'ZABAPGIT_JS_COMMON'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

lo_buf->add( '@font-face {' ).
lo_buf->add( '    font-family: "ag-icons";' ).
lo_buf->add( '    font-weight: normal;' ).
lo_buf->add( '    font-style: normal;' ).
lo_buf->add( '    src: url("../font/ag-icons.woff") format("woff");' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.icon {' ).
lo_buf->add( '    line-height: 1;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.icon:before {' ).
lo_buf->add( '    font-family: ag-icons !important;' ).
lo_buf->add( '    font-style: normal;' ).
lo_buf->add( '    font-weight: normal !important;' ).
lo_buf->add( '' ).
lo_buf->add( '    display: inline-block;' ).
lo_buf->add( '    text-decoration: none;' ).
lo_buf->add( '    text-align: center;' ).
lo_buf->add( '    vertical-align: text-top;' ).
lo_buf->add( '    /* width: 1.1em; */' ).
lo_buf->add( '    /* padding-right: 0.2em */' ).
lo_buf->add( '' ).
lo_buf->add( '    /* For safety - reset parent styles, that can break glyph codes*/' ).
lo_buf->add( '    font-variant: normal;' ).
lo_buf->add( '    text-transform: none;' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
lo_buf->add( '.icon.large { font-size: 200%; }' ).
lo_buf->add( '' ).
lo_buf->add( '.icon-abapgit:before { content: "\f101"; }' ).
lo_buf->add( '.icon-abaplint:before { content: "\f102"; }' ).
lo_buf->add( '.icon-arrow-circle-up:before { content: "\f103"; }' ).
lo_buf->add( '.icon-bars:before { content: "\f104"; }' ).
lo_buf->add( '.icon-bolt:before { content: "\f105"; }' ).
lo_buf->add( '.icon-box:before { content: "\f106"; }' ).
lo_buf->add( '.icon-briefcase:before { content: "\f107"; }' ).
lo_buf->add( '.icon-bug-solid:before { content: "\f108"; }' ).
lo_buf->add( '.icon-check:before { content: "\f109"; }' ).
lo_buf->add( '.icon-chevron-down:before { content: "\f10a"; }' ).
lo_buf->add( '.icon-chevron-left:before { content: "\f10b"; }' ).
lo_buf->add( '.icon-chevron-right:before { content: "\f10c"; }' ).
lo_buf->add( '.icon-chevron-up:before { content: "\f10d"; }' ).
lo_buf->add( '.icon-circle-dot-regular:before { content: "\f10e"; }' ).
lo_buf->add( '.icon-circle-play-regular:before { content: "\f10f"; }' ).
lo_buf->add( '.icon-circle-solid:before { content: "\f110"; }' ).
lo_buf->add( '.icon-cloud-commit:before { content: "\f111"; }' ).
lo_buf->add( '.icon-cloud-solid:before { content: "\f112"; }' ).
lo_buf->add( '.icon-cloud-upload-alt:before { content: "\f113"; }' ).
lo_buf->add( '.icon-code-branch:before { content: "\f114"; }' ).
lo_buf->add( '.icon-code-commit:before { content: "\f115"; }' ).
lo_buf->add( '.icon-code-pull-request-solid:before { content: "\f116"; }' ).
lo_buf->add( '.icon-code-solid:before { content: "\f117"; }' ).
lo_buf->add( '.icon-cog:before { content: "\f118"; }' ).
lo_buf->add( '.icon-copy-solid:before { content: "\f119"; }' ).
lo_buf->add( '.icon-download-solid:before { content: "\f11a"; }' ).
lo_buf->add( '.icon-edit-solid:before { content: "\f11b"; }' ).
lo_buf->add( '.icon-exclamation-circle:before { content: "\f11c"; }' ).
lo_buf->add( '.icon-exclamation-triangle:before { content: "\f11d"; }' ).
lo_buf->add( '.icon-file-alt:before { content: "\f11e"; }' ).
lo_buf->add( '.icon-file-code:before { content: "\f11f"; }' ).
lo_buf->add( '.icon-file-image:before { content: "\f120"; }' ).
lo_buf->add( '.icon-file:before { content: "\f121"; }' ).
lo_buf->add( '.icon-fire-alt:before { content: "\f122"; }' ).
lo_buf->add( '.icon-flow:before { content: "\f123"; }' ).
lo_buf->add( '.icon-folder:before { content: "\f124"; }' ).
lo_buf->add( '.icon-git-alt:before { content: "\f125"; }' ).
lo_buf->add( '.icon-github:before { content: "\f126"; }' ).
lo_buf->add( '.icon-heart-regular:before { content: "\f127"; }' ).
lo_buf->add( '.icon-info-circle-solid:before { content: "\f128"; }' ).
lo_buf->add( '.icon-language-solid:before { content: "\f129"; }' ).
lo_buf->add( '.icon-lock:before { content: "\f12a"; }' ).
lo_buf->add( '.icon-magnifying-glass-solid:before { content: "\f12b"; }' ).
lo_buf->add( '.icon-markdown:before { content: "\f12c"; }' ).
lo_buf->add( '.icon-paste-solid:before { content: "\f12d"; }' ).
lo_buf->add( '.icon-plug:before { content: "\f12e"; }' ).
lo_buf->add( '.icon-question-circle-solid:before { content: "\f12f"; }' ).
lo_buf->add( '.icon-redo-alt-solid:before { content: "\f130"; }' ).
lo_buf->add( '.icon-server-solid:before { content: "\f131"; }' ).
lo_buf->add( '.icon-sliders-h:before { content: "\f132"; }' ).
lo_buf->add( '.icon-snowflake:before { content: "\f133"; }' ).
lo_buf->add( '.icon-star:before { content: "\f134"; }' ).
lo_buf->add( '.icon-tag-solid:before { content: "\f135"; }' ).
lo_buf->add( '.icon-times-solid:before { content: "\f136"; }' ).
lo_buf->add( '.icon-tools-solid:before { content: "\f137"; }' ).
lo_buf->add( '.icon-truck-solid:before { content: "\f138"; }' ).
lo_buf->add( '.icon-upload-solid:before { content: "\f139"; }' ).
lo_buf->add( '.icon-user-cog-solid:before { content: "\f13a"; }' ).
lo_buf->add( '.icon-user-solid:before { content: "\f13b"; }' ).
lo_buf->add( '.icon-vial-solid:before { content: "\f13c"; }' ).
lo_buf->add( '' ).
    li_asset_man->register_asset(
      iv_url       = 'css/ag-icons.css'
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_ICON_FONT_CSS'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

    " @@abapmerge include-base64 zabapgit_icon_font.w3mi.data.woff > lo_buf->add( '$$' ).
    li_asset_man->register_asset(
      iv_url       = 'font/ag-icons.woff'
      iv_type      = 'font/woff'
      iv_mime_name = 'ZABAPGIT_ICON_FONT'
      iv_base64    = lo_buf->join_and_flush( ) ).

    ri_asset_man = li_asset_man.

  ENDMETHOD.
  METHOD get_frontend_services.

    IF gi_fe_services IS INITIAL.
      CREATE OBJECT gi_fe_services TYPE Lcl_abapgit_frontend_services.
    ENDIF.

    ri_fe_serv = gi_fe_services.

  ENDMETHOD.
  METHOD get_gui.

    DATA:
      li_hotkey_ctl TYPE REF TO Lif_abapgit_gui_hotkey_ctl,
      li_router     TYPE REF TO Lif_abapgit_gui_event_handler,
      li_asset_man  TYPE REF TO Lif_abapgit_gui_asset_manager.

    DATA lo_html_preprocessor TYPE REF TO Lcl_abapgit_gui_html_processor.

    IF go_gui IS INITIAL.
      li_asset_man = get_asset_manager( ).

      CREATE OBJECT lo_html_preprocessor EXPORTING ii_asset_man = li_asset_man.
      lo_html_preprocessor->preserve_css( 'css/ag-icons.css' ).
      lo_html_preprocessor->preserve_css( 'css/common.css' ).

      CREATE OBJECT li_router TYPE Lcl_abapgit_gui_router.
      CREATE OBJECT li_hotkey_ctl TYPE Lcl_abapgit_gui_hotkey_ctl.

      CREATE OBJECT go_gui
        EXPORTING
          io_component      = li_router
          ii_hotkey_ctl     = li_hotkey_ctl
          ii_html_processor = lo_html_preprocessor
          ii_asset_man      = li_asset_man.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.
  METHOD get_gui_services.
    IF gi_gui_services IS NOT BOUND.
      gi_gui_services ?= get_gui( ).
    ENDIF.
    ri_gui_services = gi_gui_services.
  ENDMETHOD.
  METHOD get_html_viewer.

    IF gi_html_viewer IS NOT BOUND.
      CREATE OBJECT gi_html_viewer TYPE Lcl_abapgit_html_viewer_gui
        EXPORTING
          io_container           = io_container
          iv_disable_query_table = iv_disable_query_table.
    ENDIF.

    ri_viewer = gi_html_viewer.

  ENDMETHOD.
  METHOD get_popups.

    IF gi_popups IS INITIAL.
      CREATE OBJECT gi_popups TYPE Lcl_abapgit_popups.
    ENDIF.

    ri_popups = gi_popups.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_UI_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_RONT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ront=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ront=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_RONT implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    DATA: lv_user  TYPE string,
          lx_error TYPE REF TO cx_root.

    TRY.

        SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE ront_name = ms_item-obj_name AND version = 'I'.

        IF lv_user IS INITIAL.
          SELECT SINGLE changed_by INTO lv_user
            FROM (c_table_name)
            WHERE ront_name = ms_item-obj_name AND version = 'A'.
        ENDIF.

        rv_user = lv_user.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_RONT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFBF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfbf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfbf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFBF implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    DATA: lt_bfuncts TYPE sfw_bftab,
          lt_msgtab  TYPE sprot_u_tab.

    IF Lif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_bf TO lt_bfuncts.

    cl_sfw_activate=>activate_sfbf(
      EXPORTING
        p_bfuncts = lt_bfuncts
        p_version = 'I'
      IMPORTING
        p_msgtab  = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error activating SFBF' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_bf = is_item-obj_name.

  ENDMETHOD.
  METHOD create.

    TRY.
        " make sure to clear cache
        ro_bf = cl_sfw_bf=>create_bf( mv_bf ).
        ro_bf->free( ).
        ro_bf = cl_sfw_bf=>create_bf( mv_bf ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BF=>CREATE_BF' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get.

    TRY.
        " make sure to clear cache, method GET_BF_FROM_DB does not exist in 702
        ro_bf = cl_sfw_bf=>get_bf( mv_bf ).
        ro_bf->free( ).
        ro_bf = cl_sfw_bf=>get_bf( mv_bf ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BF=>GET_BF' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_bf.

    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.

    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_bftab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_bf TO lt_delete.

    cl_sfw_activate=>delete_sfbf( EXPORTING p_bfuncts = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error deleting SFBF' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.

    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_SWITCHES'
                  CHANGING cg_data = lt_assigned_switches ).
    io_xml->read( EXPORTING iv_name = 'DEPENDANCIES'
                  CHANGING cg_data = lt_dependancies ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_KW'
                  CHANGING cg_data = ls_sfw_bfc_kw ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_TC'
                  CHANGING cg_data = ls_sfw_bfc_tc ).
    io_xml->read( EXPORTING iv_name = 'CONTENT_RN'
                  CHANGING cg_data = ls_sfw_bfc_rn ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          lo_bf = get( ).
        ELSE.
          lo_bf = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        Lcx_abapgit_exception=>raise( 'error in CL_SFW_BF=>CREATE_BF' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.

    " Get component from package
    SELECT SINGLE dlvunit FROM tdevc INTO ls_header-component WHERE devclass = iv_package.

    lo_bf->set_header_data( ls_header ).

    lo_bf->set_texts( p_32 = lv_name_32
                      p_80 = lv_name_80 ).

    lo_bf->set_assigned_switches( lt_assigned_switches ).
    lo_bf->set_excluded_bf( lt_dependancies ).
    lo_bf->set_content_data(
        im_sfw_bfc_kw = ls_sfw_bfc_kw
        im_sfw_bfc_rn = ls_sfw_bfc_rn
        im_sfw_bfc_tc = ls_sfw_bfc_tc ).
    lo_bf->set_parent_bfs( lt_parent_bfs ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_bf->save_all( ).

    unlock( ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfbf ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_tadir TYPE tadir,
          lv_bf    TYPE sfw_bfunction.

    lv_bf = ms_item-obj_name.
    IF cl_sfw_bf=>check_existence( lv_bf ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'SF' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_bf                TYPE REF TO cl_sfw_bf,
          ls_header            TYPE sfw_bf,
          lv_name_32           TYPE sfw_name32,
          lv_name_80           TYPE sfw_name80,
          lt_assigned_switches TYPE sfw_swbf_outtab,
          lt_dependancies      TYPE sfw_depend_outtab,
          ls_sfw_bfc_kw        TYPE sfw_bfc_kw,
          ls_sfw_bfc_tc        TYPE sfw_bfc_tc,
          ls_sfw_bfc_rn        TYPE sfw_bfc_rn,
          lt_parent_bfs        TYPE sfw_bs_bf_outtab.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_bf = get( ).

    ls_header = lo_bf->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-component,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bf->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_switches = lo_bf->get_assigned_switches( ).
    lt_dependancies = lo_bf->get_excluded_bf( ).
    lo_bf->get_content_data(
      IMPORTING
        ex_sfw_bfc_kw = ls_sfw_bfc_kw
        ex_sfw_bfc_tc = ls_sfw_bfc_tc
        ex_sfw_bfc_rn = ls_sfw_bfc_rn ).
    lt_parent_bfs = lo_bf->get_parent_bfs( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_switches
                 iv_name = 'ASSIGNED_SWITCHES' ).
    io_xml->add( ig_data = lt_dependancies
                 iv_name = 'DEPENDANCIES' ).
    io_xml->add( ig_data = ls_sfw_bfc_kw
                 iv_name = 'CONTENT_KW' ).
    io_xml->add( ig_data = ls_sfw_bfc_tc
                 iv_name = 'CONTENT_TC' ).
    io_xml->add( ig_data = ls_sfw_bfc_rn
                 iv_name = 'CONTENT_RN' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfbf ).

  ENDMETHOD.
  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EEUDB'
      EXPORTING
        relid     = 'SF'
        name      = ms_item-obj_name
        _synchron = 'X'
        _scope    = '1'
        mode_eudb = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SFBF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFBS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfbs=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfbs=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFBS implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    DATA: lt_bfsets TYPE sfw_bstab,
          lt_msgtab TYPE sprot_u_tab.

    IF Lif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_bfset TO lt_bfsets.

    cl_sfw_activate=>activate_sfbs(
      EXPORTING
        p_bsets   = lt_bfsets
        p_version = 'I'
      IMPORTING
        p_msgtab  = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error activating SFBS' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_bfset = is_item-obj_name.

  ENDMETHOD.
  METHOD create.

    TRY.
        " make sure to clear cache
        ro_bfs = cl_sfw_bfs=>create_bfs( mv_bfset ).
        ro_bfs->free( ).
        ro_bfs = cl_sfw_bfs=>create_bfs( mv_bfset ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BFS=>CREATE_BFS' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get.

    TRY.
        " make sure to clear cache
        ro_bfs = cl_sfw_bfs=>get_bfs( mv_bfset ).
        ro_bfs->free( ).
        ro_bfs = cl_sfw_bfs=>get_bfs( mv_bfset ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_BFS=>GET_BFS' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_bs.

    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.

    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_bstab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_bfset TO lt_delete.

    cl_sfw_activate=>delete_sfbs( EXPORTING p_bsets = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error deleting SFBS' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.

    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'ASSIGNED_BF'
                  CHANGING cg_data = lt_assigned_bf ).
    io_xml->read( EXPORTING iv_name = 'NESTED_BFS'
                  CHANGING cg_data = lt_nested_bfs ).
    io_xml->read( EXPORTING iv_name = 'PARENT_BFS'
                  CHANGING cg_data = lt_parent_bfs ).

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          lo_bfs = get( ).
        ELSE.
          lo_bfs = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        Lcx_abapgit_exception=>raise( 'error in CL_SFW_BFS=>CREATE_BFS' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_bfs->set_header_data( ls_header ).

    lo_bfs->set_texts( p_32 = lv_name_32
                       p_80 = lv_name_80 ).

    lo_bfs->set_assigned_bf( lt_assigned_bf ).
    lo_bfs->set_assigned_bfs( lt_nested_bfs ).
    lo_bfs->set_nested_parent( lt_parent_bfs ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_bfs->save_all( ).

    unlock( ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfbs ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_tadir TYPE tadir.

    IF cl_sfw_bfs=>check_existence( mv_bfset ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'SS' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_bfs         TYPE REF TO cl_sfw_bfs,
          ls_header      TYPE sfw_bs,
          lv_name_32     TYPE sfw_name32,
          lv_name_80     TYPE sfw_name80,
          lt_assigned_bf TYPE sfw_bfbs_outtab,
          lt_nested_bfs  TYPE sfw_bsbs_outtab,
          lt_parent_bfs  TYPE sfw_bs_bs_parent_outtab.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_bfs = get( ).

    ls_header = lo_bfs->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_bfs->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_assigned_bf = lo_bfs->get_assigned_bf( ).
    lt_nested_bfs = lo_bfs->get_nested_bfs( ).
    lt_parent_bfs = lo_bfs->get_nested_parent( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_assigned_bf
                 iv_name = 'ASSIGNED_BF' ).
    io_xml->add( ig_data = lt_nested_bfs
                 iv_name = 'NESTED_BFS' ).
    io_xml->add( ig_data = lt_parent_bfs
                 iv_name = 'PARENT_BFS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfbs ).

  ENDMETHOD.
  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EEUDB'
      EXPORTING
        relid     = 'SS'
        name      = ms_item-obj_name
        _synchron = 'X'
        _scope    = '1'
        mode_eudb = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SFBS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_OA2P <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_oa2p=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_oa2p=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_OA2P implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_profile = is_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lo_persist     TYPE REF TO object,
          lr_wb          TYPE REF TO data,
          lo_profile     TYPE REF TO object,
          lv_profile_key TYPE seu_objkey.

    FIELD-SYMBOLS: <lo_wb> TYPE any.


    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').

    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.

    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = lv_profile_key    " Object Key
            p_version     = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data = <lo_wb>.  " Object Data
      CATCH cx_swb_object_does_not_exist.
        Lcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    lo_profile = <lo_wb>.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~GET_CHANGED_BY')
      RECEIVING
        p_user_name = rv_user.


  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CONSTANTS: lc_actvt TYPE c LENGTH 2 VALUE `06`.

    DATA: lo_persist     TYPE REF TO object,
          lv_profile_key TYPE seu_objkey.

    "authority check
    AUTHORITY-CHECK OBJECT 'S_OA2C_ADM'
      ID 'ACTVT'     FIELD lc_actvt.
    IF sy-subrc <> 0.
      MESSAGE e463(01) WITH mv_profile INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    "delete profile
    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').

    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~DELETE')
          EXPORTING
            p_object_key = lv_profile_key.   " Object Key
      CATCH cx_swb_object_does_not_exist.
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error when deleting OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_persist      TYPE REF TO object,
          lo_profile      TYPE REF TO object,
          lr_wb           TYPE REF TO data,
          lr_profile_data TYPE REF TO data.
    FIELD-SYMBOLS: <ls_profile_data> TYPE data,
                   <lo_wb>           TYPE any.

    CREATE DATA lr_profile_data TYPE ('OA2C_SX_OA2P_OBJECT_DATA').
    ASSIGN lr_profile_data->* TO <ls_profile_data>.

    io_xml->read( EXPORTING iv_name = 'PROFILE'
                  CHANGING cg_data = <ls_profile_data> ).



    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.

    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_DATA')
      EXPORTING
        p_data = <ls_profile_data>.

    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').
    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~SAVE')
          EXPORTING
            p_object_data = <lo_wb>.   " Object Data
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error deserialize profile { mv_profile }.| ).
    ENDTRY.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD ('CL_OA2P_OBJECT_PERSIST')=>('CHECK_EXISTS_ON_DB')
      EXPORTING
        i_profile = mv_profile
      RECEIVING
        r_exists  = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_profile_name TYPE eqegraarg,
          lv_lock_number  TYPE i,
          lt_locks        TYPE STANDARD TABLE OF seqg3.

    lv_profile_name = mv_profile.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient = sy-mandt    " Client
        gname   = 'OA2C_PROFILES'    " Granularity name (-> table name)
        garg    = lv_profile_name    " Granularity value(->values of key fields)
      IMPORTING
        number  = lv_lock_number
      TABLES
        enq     = lt_locks.    " Number of chosen lock entries


    rv_is_locked = boolc( lv_lock_number > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_persist      TYPE REF TO object,
          lo_profile      TYPE REF TO object,
          lv_profile_key  TYPE seu_objkey,
          lr_profile_data TYPE REF TO data,
          lr_wb           TYPE REF TO data.

    FIELD-SYMBOLS: <ls_profile_data> TYPE data,
                   <lo_specifics>    TYPE any,
                   <lo_wb>           TYPE any.

    CREATE DATA lr_profile_data TYPE ('OA2C_SX_OA2P_OBJECT_DATA').
    ASSIGN lr_profile_data->* TO <ls_profile_data>.


    lv_profile_key = mv_profile.
    CREATE OBJECT lo_persist TYPE ('CL_OA2P_OBJECT_PERSIST').
    CREATE OBJECT lo_profile TYPE ('CL_OA2P_OBJECT_DATA').
    CREATE DATA lr_wb TYPE REF TO ('IF_WB_OBJECT_DATA_MODEL').
    ASSIGN lr_wb->* TO <lo_wb>.
    <lo_wb> ?= lo_profile.


    TRY.
        CALL METHOD lo_persist->('IF_WB_OBJECT_PERSIST~GET')
          EXPORTING
            p_object_key  = lv_profile_key    " Object Key
            p_version     = 'A'    " Version (Active/Inactive)
          CHANGING
            p_object_data = <lo_wb>.  " Object Data
      CATCH cx_swb_object_does_not_exist.
        Lcx_abapgit_exception=>raise( |OAuth2 Profile { lv_profile_key } doesn't exist.| ).
      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error when geting details of OAuth2 Profile { lv_profile_key }.| ).
    ENDTRY.

    "remove system specific information
    lo_profile = <lo_wb>.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CHANGED_BY')
      EXPORTING
        p_user_name = ''.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CHANGED_ON')
      EXPORTING
        p_date = '00000000'
        p_time = '000000'.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CREATED_BY')
      EXPORTING
        p_user_name = ''.
    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~SET_CREATED_ON')
      EXPORTING
        p_date = '00000000'
        p_time = '000000'.

    CALL METHOD lo_profile->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      IMPORTING
        p_data = <ls_profile_data>.

    "remove runtime information
    ASSIGN COMPONENT 'O_SPECIFICS' OF STRUCTURE <ls_profile_data> TO <lo_specifics>.
    CLEAR <lo_specifics>.

    io_xml->add( iv_name = 'PROFILE'
                 ig_data = <ls_profile_data> ).


  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_OA2P implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFSW <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfsw=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfsw=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFSW implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    DATA: lt_switches TYPE sfw_switchtab,
          lt_msgtab   TYPE sprot_u_tab.

    IF Lif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_switch TO lt_switches.

    cl_sfw_activate=>activate_sfsw(
      EXPORTING
        p_switches = lt_switches
        p_version  = 'I'
      IMPORTING
        p_msgtab   = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error activating SFBS' ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_switch = is_item-obj_name.

  ENDMETHOD.
  METHOD create.

    TRY.
        " make sure to clear cache
        ro_switch = cl_sfw_sw=>create_switch( mv_switch ).
        ro_switch->free( ).
        ro_switch = cl_sfw_sw=>create_switch( mv_switch ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_SW=>CREATE_SWITCH' ).
    ENDTRY.

  ENDMETHOD.
  METHOD get.

    TRY.
        " make sure to clear cache
        ro_switch = cl_sfw_sw=>get_switch( mv_switch ).
        ro_switch->free( ).
        ro_switch = cl_sfw_sw=>get_switch( mv_switch ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        Lcx_abapgit_exception=>raise( 'Error from CL_SFW_SW=>GET_SWITCH' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_switch.


    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.
    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_switchtab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_switch TO lt_delete.

    cl_sfw_activate=>delete_sfsw( EXPORTING p_switches = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( 'Error deleting SFSW' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.

    IF iv_step = Lif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'PARENT_BF'
                  CHANGING cg_data = lt_parent_bf ).
    io_xml->read( EXPORTING iv_name = 'CONFLICTS'
                  CHANGING cg_data = lt_conflicts ).

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          lo_switch = get( ).
        ELSE.
          lo_switch = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        Lcx_abapgit_exception=>raise( 'error in CL_SFW_SW=>CREATE_SWITCH' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_switch->set_header_data( ls_header ).

    lo_switch->set_texts( p_32 = lv_name_32
                          p_80 = lv_name_80 ).

    lo_switch->set_parent_bf( lt_parent_bf ).
    lo_switch->set_conflicts( lt_conflicts ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_switch->save_all(
      EXCEPTIONS
        not_saved = 1
        OTHERS    = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error in CL_SFW_SW->SAVE_ALL' ).
    ENDIF.

    unlock( ).

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfsw ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_tadir TYPE tadir.

    IF cl_sfw_sw=>check_existence( mv_switch ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'SW' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_switch = get( ).

    ls_header = lo_switch->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_switch->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_parent_bf = lo_switch->get_parent_bf( ).
    lt_conflicts = lo_switch->get_conflicts( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_parent_bf
                 iv_name = 'PARENT_BF' ).
    io_xml->add( ig_data = lt_conflicts
                 iv_name = 'CONFLICTS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfsw ).

  ENDMETHOD.
  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EEUDB'
      EXPORTING
        relid     = 'SW'
        name      = ms_item-obj_name
        _synchron = 'X'
        _scope    = '1'
        mode_eudb = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SFSW implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SAXX_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_saxx_super=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_saxx_super=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SAXX_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD create_channel_objects.

    get_names( ).

    TRY.
        IF mi_appl_obj_data IS NOT BOUND.
          CREATE OBJECT mi_appl_obj_data TYPE (mv_appl_obj_cls_name).
        ENDIF.

        IF mi_persistence IS NOT BOUND.
          CREATE OBJECT mi_persistence TYPE (mv_persistence_cls_name).
        ENDIF.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_data.

    DATA: lv_object_key TYPE seu_objkey.

    lv_object_key = ms_item-obj_name.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = lv_object_key
            p_version     = 'A'
          CHANGING
            p_object_data = mi_appl_obj_data ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    mi_appl_obj_data->get_data( IMPORTING p_data = eg_data ).

  ENDMETHOD.
  METHOD get_names.

    IF mv_data_structure_name IS INITIAL.
      mv_data_structure_name  = get_data_structure_name( ).
    ENDIF.

    IF mv_appl_obj_cls_name IS INITIAL.
      mv_appl_obj_cls_name    = get_data_class_name( ).
    ENDIF.

    IF mv_persistence_cls_name IS INITIAL.
      mv_persistence_cls_name = get_persistence_class_name( ).
    ENDIF.

  ENDMETHOD.
  METHOD lock.

    DATA: lv_objname    TYPE trobj_name,
          lv_object_key TYPE seu_objkey,
          lv_objtype    TYPE trobjtype.


    lv_objname    = ms_item-obj_name.
    lv_object_key = ms_item-obj_name.
    lv_objtype    = ms_item-obj_type.

    mi_persistence->lock(
      EXPORTING
        p_objname_tr   = lv_objname
        p_object_key   = lv_object_key
        p_objtype_tr   = lv_objtype
      EXCEPTIONS
        foreign_lock   = 1
        error_occurred = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error occured while locking { ms_item-obj_type } { lv_objname }| ).
    ENDIF.

  ENDMETHOD.
  METHOD unlock.

    DATA: lv_objname    TYPE trobj_name,
          lv_object_key TYPE seu_objkey,
          lv_objtype    TYPE trobjtype.

    lv_objname    = ms_item-obj_name.
    lv_object_key = ms_item-obj_name.
    lv_objtype    = ms_item-obj_type.

    mi_persistence->unlock( p_objname_tr = lv_objname
                            p_object_key = lv_object_key
                            p_objtype_tr = lv_objtype ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>       TYPE any,
                   <lg_header>     TYPE any,
                   <lg_changed_by> TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_name } not supported| ).
    ENDTRY.

    get_data( IMPORTING eg_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
    ASSERT sy-subrc = 0.
    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_changed_by>.
    ASSERT sy-subrc = 0.

    IF <lg_changed_by> IS NOT INITIAL.
      rv_user = <lg_changed_by>.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_object_key TYPE seu_objkey.

    create_channel_objects( ).

    lv_object_key = ms_item-obj_name.

    TRY.
        lock( ).

        mi_persistence->delete( lv_object_key ).

        unlock( ).

      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error occured while deleting { ms_item-obj_type }| ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_data TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data> TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    io_xml->read(
      EXPORTING
        iv_name = ms_item-obj_type
      CHANGING
        cg_data = <lg_data> ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    TRY.
        lock( ).

        corr_insert( iv_package ).

        mi_appl_obj_data->set_data( <lg_data> ).

        mi_persistence->save( mi_appl_obj_data ).

        unlock( ).

      CATCH cx_swb_exception.
        Lcx_abapgit_exception=>raise( |Error occured while creating { ms_item-obj_type }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_key TYPE seu_objkey.

    create_channel_objects( ).

    lv_object_key = ms_item-obj_name.

    TRY.
        mi_persistence->get( p_object_key           = lv_object_key
                             p_version              = 'A'
                             p_existence_check_only = abap_true ).

      CATCH cx_swb_object_does_not_exist cx_swb_exception.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lr_data             TYPE REF TO data.

    FIELD-SYMBOLS: <lg_data>   TYPE any,
                   <lg_header> TYPE any,
                   <lg_field>  TYPE any.

    create_channel_objects( ).

    TRY.
        CREATE DATA lr_data TYPE (mv_data_structure_name).
        ASSIGN lr_data->* TO <lg_data>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |{ ms_item-obj_type } not supported| ).
    ENDTRY.

    get_data( IMPORTING eg_data = <lg_data> ).

    ASSIGN COMPONENT 'HEADER' OF STRUCTURE <lg_data> TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CHANGED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CHANGED_CLNT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_ON' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_BY' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_AT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    ASSIGN COMPONENT 'CREATED_CLNT' OF STRUCTURE <lg_header> TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

    io_xml->add( iv_name = ms_item-obj_type
                 ig_data = <lg_data> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SAXX_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SCP1 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_scp1=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_scp1=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SCP1 implementation.
*"* method's implementations
*include methods.
  METHOD adjust_inbound.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca,
                   <ls_scprvall> TYPE scprvall.

* back to internal format
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      SHIFT <ls_scprvals>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      SHIFT <ls_scprreca>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.
    LOOP AT cs_scp1-scprvall ASSIGNING <ls_scprvall>.
      SHIFT <ls_scprvall>-recnumber RIGHT DELETING TRAILING space.
    ENDLOOP.

  ENDMETHOD.
  METHOD adjust_outbound.

    FIELD-SYMBOLS: <ls_scprvals> TYPE scprvals,
                   <ls_scprreca> TYPE scprreca,
                   <ls_scprvall> TYPE scprvall.

* normalize the XML
    LOOP AT cs_scp1-scprvals ASSIGNING <ls_scprvals>.
      CONDENSE <ls_scprvals>-recnumber.
    ENDLOOP.
    LOOP AT cs_scp1-scprreca ASSIGNING <ls_scprreca>.
      CONDENSE <ls_scprreca>-recnumber.
    ENDLOOP.
    LOOP AT cs_scp1-scprvall ASSIGNING <ls_scprvall>.
      CONDENSE <ls_scprvall>-recnumber.
    ENDLOOP.

  ENDMETHOD.
  METHOD call_delete_fms.

    CONSTANTS:
      lc_version_new      TYPE c VALUE 'N', "Include SCPRINTCONST version_new
      lc_operation_delete TYPE c VALUE 'D'.

    DATA:
      lv_profile_type   TYPE scprattr-type,
      lt_fatherprofiles TYPE STANDARD TABLE OF scproprof WITH DEFAULT KEY,
      ls_fatherprofile  TYPE scproprof.


    CALL FUNCTION 'SCPR_DB_ATTR_GET_DETAIL'
      EXPORTING
        profid   = iv_profile_id
        version  = lc_version_new
      IMPORTING
        proftype = lv_profile_type
      EXCEPTIONS
        OTHERS   = 0.

    CALL FUNCTION 'SCPR_PRSET_DB_USED_IN'
      EXPORTING
        profid   = iv_profile_id
        version  = lc_version_new
      TABLES
        profiles = lt_fatherprofiles.

    ls_fatherprofile-id = iv_profile_id.
    APPEND ls_fatherprofile TO lt_fatherprofiles.
    CALL FUNCTION 'SCPR_CT_TRANSPORT_ENTRIES'
      TABLES
        profids                  = lt_fatherprofiles
      EXCEPTIONS
        error_in_transport_layer = 1
        user_abort               = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error while deleting SCP1 - TRANSPORT, { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'SCPR_PRSET_DB_DELETE_ALL'
      EXPORTING
        profid      = iv_profile_id
        proftype    = lv_profile_type
      TABLES
        fatherprofs = lt_fatherprofiles
      EXCEPTIONS
        user_abort  = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error while deleting SCP1 - DB_DELETE, { sy-subrc }| ).
    ENDIF.

    CALL FUNCTION 'SCPR_MEM_SCPR_ACTIONS_ADD'
      EXPORTING
        bcset_id  = iv_profile_id
        operation = lc_operation_delete.

  ENDMETHOD.
  METHOD dequeue.

    DATA: lv_id TYPE scpr_id.


    lv_id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_SV_DEQUEUE_BCSET'
      EXPORTING
        bcset_id = lv_id.

  ENDMETHOD.
  METHOD enqueue.

    DATA: lv_id TYPE scpr_id.

    lv_id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_SV_ENQUEUE_BCSET'
      EXPORTING
        bcset_id          = lv_id
      EXCEPTIONS
        is_already_locked = 1
        system_failure    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD load.

    CALL FUNCTION 'SCPR_TEMPL_DB_VALS_GET_DETAIL'
      EXPORTING
        profid   = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        values   = cs_scp1-scprvals
        valuesl  = cs_scp1-scprvall
        recattr  = cs_scp1-scprreca.

    CALL FUNCTION 'SCPR_TEMPL_DB_FLDTXTVAR_GET'
      EXPORTING
        bcset_id = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        it_fldv  = cs_scp1-scprfldv.

  ENDMETHOD.
  METHOD load_hier.

    CALL FUNCTION 'SCPR_PRSET_DB_SUBP_GET_DETAIL'
      EXPORTING
        profid   = cs_scp1-scprattr-id
        category = cs_scp1-scprattr-category
      TABLES
        subprofs = cs_scp1-subprofs.

  ENDMETHOD.
  METHOD save.

    DATA: ls_scp1 TYPE ty_scp1,
          ls_text TYPE scprtext.


* copy everything to local, the function module changes the values
    ls_scp1 = is_scp1.

    READ TABLE ls_scp1-scprtext INTO ls_text WITH KEY langu = mv_language. "#EC CI_SUBRC

    CALL FUNCTION 'SCPR_TEMPL_MN_TEMPLATE_SAVE'
      EXPORTING
        profid                    = ls_scp1-scprattr-id
        proftext                  = ls_text-text
        category                  = ls_scp1-scprattr-category
        cli_dep                   = ls_scp1-scprattr-cli_dep
        cli_cas                   = ls_scp1-scprattr-cli_cas
        reftype                   = ls_scp1-scprattr-reftype
        refname                   = ls_scp1-scprattr-refname
        orgid                     = ls_scp1-scprattr-orgid
        component                 = ls_scp1-scprattr-component
        minrelease                = ls_scp1-scprattr-minrelease
        maxrelease                = ls_scp1-scprattr-maxrelease
        act_info                  = ls_scp1-scprattr-act_info
        bcset_type                = ls_scp1-scprattr-type
        fldtxtvar_supplied        = 'YES'
        with_transp_insert        = abap_false
        with_progress_indicator   = abap_false
        remove_denied_data        = abap_true
        ask_for_cont_after_remove = abap_true
      TABLES
        values                    = ls_scp1-scprvals
        valuesl                   = ls_scp1-scprvall
        recattr                   = ls_scp1-scprreca
        it_fldv                   = ls_scp1-scprfldv
        texts                     = ls_scp1-scprtext
      EXCEPTIONS
        user_abort                = 1
        error_in_transport_layer  = 2
        inconsistent_data         = 3
        database_error            = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD save_hier.

    DATA: ls_scp1  TYPE ty_scp1,
          ls_profs LIKE LINE OF ls_scp1-subprofs,
          lt_sub   TYPE STANDARD TABLE OF scproprof WITH DEFAULT KEY,
          ls_sub   LIKE LINE OF lt_sub,
          ls_text  TYPE scprtext.


* copy everything to local, the function module changes the values
    ls_scp1 = is_scp1.

    READ TABLE ls_scp1-scprtext INTO ls_text WITH KEY langu = mv_language. "#EC CI_SUBRC

* see fm SCPR_PRSET_DB_STORE, only this field and sequence is used
    LOOP AT ls_scp1-subprofs INTO ls_profs.
      ls_sub-id = ls_profs-subprofile.
      APPEND ls_sub TO lt_sub.
    ENDLOOP.

    CALL FUNCTION 'SCPR_PRSET_MN_BCSET_SAVE'
      EXPORTING
        profid                   = ls_scp1-scprattr-id
        proftext                 = ls_text-text
        category                 = ls_scp1-scprattr-category
        cli_dep                  = ls_scp1-scprattr-cli_dep
        cli_cas                  = ls_scp1-scprattr-cli_cas
        reftype                  = ls_scp1-scprattr-reftype
        refname                  = ls_scp1-scprattr-refname
        orgid                    = ls_scp1-scprattr-orgid
        component                = ls_scp1-scprattr-component
        minrelease               = ls_scp1-scprattr-minrelease
        maxrelease               = ls_scp1-scprattr-maxrelease
        act_info                 = ls_scp1-scprattr-act_info
        with_transp_insert       = abap_false
        with_progress_indicator  = abap_false
      TABLES
        subprofs                 = lt_sub
        texts                    = ls_scp1-scprtext
      EXCEPTIONS
        user_abort               = 1
        error_in_transport_layer = 2
        OTHERS                   = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE modifier INTO rv_user FROM scprattr
      WHERE id = ms_item-obj_name
      AND version = 'N'.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_profile_id TYPE scpr_id.

    lv_profile_id = ms_item-obj_name.

    enqueue( ).
    call_delete_fms( lv_profile_id ).
    dequeue( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_scp1 TYPE ty_scp1.


    io_xml->read(
      EXPORTING iv_name = 'SCP1'
      CHANGING  cg_data = ls_scp1 ).

    adjust_inbound( CHANGING cs_scp1 = ls_scp1 ).

    IF ls_scp1-scprattr-type = 'TMP'.
      save_hier( ls_scp1 ).
    ELSE.
      save( ls_scp1 ).
    ENDIF.

    dequeue( ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_rc     TYPE sy-subrc,
          lv_profid TYPE scprattr-id.


    lv_profid = ms_item-obj_name.

    CALL FUNCTION 'SCPR_BCSET_EXISTS'
      EXPORTING
        profid = lv_profid
      IMPORTING
        rc     = lv_rc.
    rv_bool = boolc( lv_rc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lv_display_only TYPE scpr_txt20,
          lv_bcset_id     TYPE scpr_id.

    lv_display_only = abap_false.
    lv_bcset_id     = ms_item-obj_name.

    EXPORT scpr3_display_only = lv_display_only
           scpr3_bcset_id     = lv_bcset_id
        TO MEMORY ID 'SCPR3_PARAMETER'.

    SUBMIT scpr3 AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_scp1 TYPE ty_scp1.


    ls_scp1-scprattr-id = ms_item-obj_name.

    CALL FUNCTION 'SCPR_DB_ATTR_GET_DETAIL'
      EXPORTING
        profid     = ls_scp1-scprattr-id
      IMPORTING
        proftype   = ls_scp1-scprattr-type
        cli_dep    = ls_scp1-scprattr-cli_dep
        cli_cas    = ls_scp1-scprattr-cli_cas
        reftype    = ls_scp1-scprattr-reftype
        refname    = ls_scp1-scprattr-refname
        component  = ls_scp1-scprattr-component
        minrelease = ls_scp1-scprattr-minrelease
        maxrelease = ls_scp1-scprattr-maxrelease
        orgid      = ls_scp1-scprattr-orgid
        act_info   = ls_scp1-scprattr-act_info.

    CALL FUNCTION 'SCPR_TEXT_GET'
      EXPORTING
        profid        = ls_scp1-scprattr-id
        category      = ls_scp1-scprattr-category
      TABLES
        texts         = ls_scp1-scprtext
      EXCEPTIONS
        no_text_found = 1.

    IF ls_scp1-scprattr-type = 'TMP'.
      load_hier( CHANGING cs_scp1 = ls_scp1 ).
    ELSE.
      load( CHANGING cs_scp1 = ls_scp1 ).
    ENDIF.

    adjust_outbound( CHANGING cs_scp1 = ls_scp1 ).

    io_xml->add(
      iv_name = 'SCP1'
      ig_data  = ls_scp1 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SCP1 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SRVD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_srvd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_srvd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SRVD implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata TO <lv_value>.
    IF sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDIF.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname = 'VERSION'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_AT'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CREATED_BY'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_AT'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'CHANGED_BY'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'RESPONSIBLE'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'PACKAGE_REF'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'MASTER_SYSTEM'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'DT_UUID'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGUAGE_VERSION'
      CHANGING
        cs_metadata  = cs_metadata ).
    clear_field(
      EXPORTING
        iv_fieldname = 'ABAP_LANGU_VERSION'
      CHANGING
        cs_metadata  = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname = 'LINKS'
      CHANGING
        cs_metadata  = cs_metadata ).

  ENDMETHOD.
  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_service_definition_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_service_definition TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |SRVD not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_object_data.

    DATA:
      lr_metadata TYPE REF TO data,
      lr_data     TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_metadata_node> TYPE any,
      <ls_metadata>      TYPE any,
      <lv_source>        TYPE any,
      <lg_data>          TYPE any.

    CREATE DATA lr_data TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE <lg_data> TO <lv_metadata_node>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_metadata  TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_METADATA_EXTENDED').
    ASSIGN lr_metadata->* TO <ls_metadata>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = c_xml_parent_name
      CHANGING
        cg_data = <ls_metadata> ).

    <lv_metadata_node> = <ls_metadata>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lv_source>.
    ASSERT sy-subrc = 0.

    <lv_source> = Lif_abapgit_object~mo_files->read_string( c_source_file ).
    IF <lv_source> IS INITIAL.
      <lv_source> = Lif_abapgit_object~mo_files->read_string( 'assrvd' ).
    ENDIF.

    CREATE OBJECT ro_object_data TYPE ('CL_SRVD_WB_OBJECT_DATA').
    ro_object_data->set_data( p_data = <lg_data> ).

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mo_object_operator IS BOUND.
      ro_object_operator = mo_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'SRVD'.
    ls_object_type-subtype_wb = 'SRV'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_service_definition_key
          RECEIVING
            result      = mo_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ro_object_operator = mo_object_operator.

  ENDMETHOD.
  METHOD merge_object_data.

    DATA:
      lo_object_data        TYPE REF TO object,
      lo_object_data_old    TYPE REF TO if_wb_object_data_model,
      lr_new                TYPE REF TO data,
      lr_old                TYPE REF TO data,
      lo_wb_object_operator TYPE REF TO object.

    FIELD-SYMBOLS:
      <ls_new>       TYPE any,
      <ls_old>       TYPE any,
      <lv_field_old> TYPE any,
      <lv_field_new> TYPE any.

    CREATE OBJECT lo_object_data TYPE ('CL_SRVD_WB_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_SRVD_WB_OBJECT_DATA=>TY_SRVD_OBJECT_DATA').
    ASSIGN lr_old->* TO <ls_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD lo_object_data->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL'
      IMPORTING
        p_data           = <ls_new>.

    lo_wb_object_operator = get_wb_object_operator( ).

    CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      EXPORTING
        data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        eo_object_data = lo_object_data_old.

    CALL METHOD lo_object_data_old->('GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        p_data           = <ls_old>.

    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    CREATE OBJECT ro_object_data_merged TYPE ('CL_SRVD_WB_OBJECT_DATA').

    CALL METHOD ro_object_data_merged->('SET_DATA')
      EXPORTING
        p_data = <ls_old>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA:
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lo_wb_object_operator TYPE REF TO object,
      lo_merged_data_all    TYPE REF TO if_wb_object_data_model,
      lo_merged_data_prop   TYPE REF TO if_wb_object_data_model,
      lo_merged_data_cont   TYPE REF TO if_wb_object_data_model,
      lr_wbobjtype          TYPE REF TO data,
      lr_category           TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_wbobjtype> TYPE any,
      <lv_category>  TYPE any,
      <lv_field>     TYPE any.

    TRY.
        lo_object_data = get_object_data( io_xml ).
        lo_wb_object_operator = get_wb_object_operator( ).

        CREATE DATA lr_wbobjtype TYPE ('WBOBJTYPE').
        ASSIGN lr_wbobjtype->* TO <ls_wbobjtype>.
        ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'SRVD'.
        ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'SRV'.

        CREATE DATA lr_category TYPE ('WBADT_RESOURCE_CATEGORY').
        ASSIGN lr_category->* TO <lv_category>.

        CALL METHOD ('CL_BLUE_WB_UTILITY')=>('GET_RESOURCE_CATEGORY')
          EXPORTING
            is_object_type = <ls_wbobjtype>
          RECEIVING
            result         = <lv_category>.

        lo_wb_object_operator = get_wb_object_operator( ).

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_false.
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I' "swbm_version_inactive
                  package           = iv_package
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I' "swbm_version_inactive
                  package           = iv_package
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
            WHEN OTHERS.
              Lcx_abapgit_exception=>raise( |Category '{ <lv_category> }' not supported| ).
          ENDCASE.
        ELSE.
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              lo_merged_data_all = merge_object_data( lo_object_data ).
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_merged_data_all
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              lo_merged_data_prop = merge_object_data( lo_object_data ).
              lo_merged_data_cont = merge_object_data( lo_object_data ).
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_merged_data_prop
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_merged_data_cont
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I' "swbm_version_inactive
                  transport_request = iv_transport.
            WHEN OTHERS.
              Lcx_abapgit_exception=>raise( |Category '{ <lv_category> }' not supported| ).
          ENDCASE.
        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lo_object_data TYPE REF TO if_wb_object_data_model.
    DATA lo_wb_object_operator TYPE REF TO object.

    TRY.
        lo_wb_object_operator = get_wb_object_operator( ).
        CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            data_selection = 'P'
          IMPORTING
            eo_object_data = lo_object_data.
        rv_bool = boolc( lo_object_data IS NOT INITIAL AND lo_object_data->get_object_key( ) IS NOT INITIAL ).
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_service_definition> TYPE any,
      <lv_metadata>           TYPE any,
      <lv_source>             TYPE string.

    ASSIGN mr_service_definition->* TO <ls_service_definition>.
    ASSERT sy-subrc = 0.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
            data_selection = 'AL'
          IMPORTING
            "data           = <ls_service_definition>
            eo_object_data = li_object_data_model.

        CALL METHOD li_object_data_model->('GET_DATA')
          IMPORTING
            p_data = <ls_service_definition>.

        ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_service_definition> TO <lv_metadata>.
        ASSERT sy-subrc = 0.
        clear_fields( CHANGING cs_metadata = <lv_metadata> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_service_definition> TO <lv_source>.
        ASSERT sy-subrc = 0.
        lv_source = <lv_source>.

        io_xml->add(
          iv_name = c_xml_parent_name
          ig_data = <lv_metadata> ).

        Lif_abapgit_object~mo_files->add_string(
          iv_ext    = c_source_file
          iv_string = lv_source ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SRVD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFPF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfpf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfpf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFPF implementation.
*"* method's implementations
*include methods.
  METHOD fix_oref.

* During serialization of a SFPF / SFPI object the interface hierarchy
* is represented by attributes "id" and "href", where the id looks
* like "o<number>" and href like "#o<number>". Every run of
* serialization generates a new <number> in these  attributes, that
* leads to differences even by comparing of untouched forms.
* The purpose of this method is to renumber the id's consequentially
* and therefore to avoid fictive differences.

* NB: As the method iterator->get_next() works quite slowly,
*     it is better to collect all attributes in a cache table
*     instead of implementing of a nested loop using get_next().

    DATA:
      li_iterator TYPE REF TO if_ixml_node_iterator,
      li_elem     TYPE REF TO if_ixml_element,
      lv_new      TYPE string,
      lv_old      TYPE string,
      lv_count    TYPE i,
      BEGIN OF ls_attr_href,
        val  TYPE string,
        attr TYPE REF TO if_ixml_attribute,
      END OF ls_attr_href,
      lt_attr_href LIKE SORTED TABLE OF ls_attr_href WITH NON-UNIQUE KEY val.

    FIELD-SYMBOLS <ls_attr_href> LIKE LINE OF lt_attr_href.

*   Collect all attributes href='#o...' in the cache table
    li_iterator = ii_document->create_iterator_filtered(
      ii_document->create_filter_and(
        filter1 = ii_document->create_filter_node_type( if_ixml_node=>co_node_element )
        filter2 = ii_document->create_filter_attribute( 'href' ) ) ).
    li_elem ?= li_iterator->get_next( ).
    WHILE li_elem IS NOT INITIAL.
      ls_attr_href-attr = li_elem->get_attribute_node( 'href' ).
      ls_attr_href-val = ls_attr_href-attr->get_value( ).
      IF ls_attr_href-val CP '##o*'.
        INSERT ls_attr_href INTO TABLE lt_attr_href.
      ENDIF.
      li_elem ?= li_iterator->get_next( ).
    ENDWHILE.

*   Renumber id='o...' attributes
    li_iterator = ii_document->create_iterator_filtered(
      ii_document->create_filter_and(
        filter1 = ii_document->create_filter_node_type( if_ixml_node=>co_node_element )
        filter2 = ii_document->create_filter_attribute( 'id' ) ) ).
    li_elem ?= li_iterator->get_next( ).
    WHILE li_elem IS NOT INITIAL.
      lv_old = li_elem->get_attribute( 'id' ).
      IF lv_old CP 'o*'.
        lv_count = lv_count + 1.
        lv_new = |o{ lv_count }|.
*       Rewrite id
        IF li_elem->set_attribute( name = 'id'
                                   value = lv_new ) IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( 'SFPF error, FIX_OREF' ).
        ENDIF.
*       Update references
        LOOP AT lt_attr_href ASSIGNING <ls_attr_href> WHERE val = '#' && lv_old.
          IF <ls_attr_href>-attr->set_value( '#' && lv_new ) IS NOT INITIAL.
            Lcx_abapgit_exception=>raise( 'SFPF error, FIX_OREF' ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      li_elem ?= li_iterator->get_next( ).
    ENDWHILE.

  ENDMETHOD.
  METHOD form_to_xstring.

    CONSTANTS: lc_empty_data TYPE xstring VALUE ''.

    DATA: li_fp_form     TYPE REF TO if_fp_form,
          li_wb_form     TYPE REF TO if_fp_wb_form,
          li_fp_layout   TYPE REF TO if_fp_layout,
          lx_fp_err      TYPE REF TO cx_fp_api,
          lx_fp_conv_err TYPE REF TO cx_fp_api,
          lv_layout_data TYPE xstring.

    li_wb_form = load( ).
    li_fp_form ?= li_wb_form->get_object( ).
    li_fp_layout = li_fp_form->get_layout( ).
    lv_layout_data = li_fp_layout->get_layout_data( ).

    Lif_abapgit_object~mo_files->add_raw(
      iv_ext  = c_layout_file_ext
      iv_data = lv_layout_data ).

    TRY.
        li_fp_layout->set_layout_data( i_layout_data   = lc_empty_data
                                       i_set_xliff_ids = abap_false ).
      CATCH cx_fp_api INTO lx_fp_err.
        Lcx_abapgit_exception=>raise( |SFPF remove layout: { lx_fp_err->get_text( ) }| ).
    ENDTRY.

    TRY.
        rv_xstr = cl_fp_helper=>convert_form_to_xstring( li_fp_form ).
      CATCH cx_fp_api INTO lx_fp_conv_err.
        " Pass - the exception is handled below!
    ENDTRY.

    TRY.
        li_fp_layout->set_layout_data( i_layout_data   = lv_layout_data
                                       i_set_xliff_ids = abap_false ).
      CATCH cx_fp_api INTO lx_fp_err.
        " Be aware that there might be another exception
        " raised by cl_fp_helper=>convert_form_to_xstring( )
        Lcx_abapgit_exception=>raise( |SFPF recover layout: { lx_fp_err->get_text( ) }| ).
    ENDTRY.

    IF lx_fp_conv_err IS BOUND.
      " This statement handles the exception raised from cl_fp_helper=>convert_form_to_xstring( )
      Lcx_abapgit_exception=>raise( |SFPF convert_form_to_xstring: { lx_fp_conv_err->get_text( ) }| ).
    ENDIF.
  ENDMETHOD.
  METHOD load.

    DATA: lv_name TYPE fpname.


    lv_name = ms_item-obj_name.

    TRY.
        ri_wb_form = cl_fp_wb_form=>load( lv_name ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPF error, load' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM fplayout
      INTO rv_user
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    IF rv_user IS INITIAL.
      SELECT SINGLE firstuser FROM fplayout
        INTO rv_user
        WHERE name = ms_item-obj_name
        AND state = 'A'.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name TYPE fpname.

    lv_name = ms_item-obj_name.

    TRY.
        TRY.
            CALL METHOD cl_fp_wb_form=>('DELETE')
              EXPORTING
                i_name     = lv_name
                i_ordernum = iv_transport
                i_dark     = abap_true. " > 740
          CATCH cx_sy_dyn_call_error.
            cl_fp_wb_form=>delete(
              i_name     = lv_name
              i_ordernum = iv_transport ).
        ENDTRY.
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, delete' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_xstr      TYPE xstring,
          lv_layout    TYPE xstring,
          lv_name      TYPE fpname,
          li_wb_object TYPE REF TO if_fp_wb_form,
          li_form      TYPE REF TO if_fp_form,
          lx_fp_err    TYPE REF TO cx_fp_api.


    lv_name = ms_item-obj_name.
    lv_xstr = cl_ixml_80_20=>render_to_xstring( io_xml->get_raw( ) ).

    TRY.
        li_form = cl_fp_helper=>convert_xstring_to_form( lv_xstr ).

        IF Lif_abapgit_object~mo_files->contains_file( c_layout_file_ext ) = abap_true.
          lv_layout = Lif_abapgit_object~mo_files->read_raw( c_layout_file_ext ).
          li_form->get_layout( )->set_layout_data( lv_layout ).
        ENDIF.

        IF Lif_abapgit_object~exists( ) = abap_true.
          TRY.
              CALL METHOD cl_fp_wb_form=>('DELETE')
                EXPORTING
                  i_name     = lv_name
                  i_ordernum = iv_transport
                  i_dark     = abap_true. " > 740
            CATCH cx_sy_dyn_call_error.
              cl_fp_wb_form=>delete(
                i_name     = lv_name
                i_ordernum = iv_transport ).
          ENDTRY.
        ENDIF.

        tadir_insert( iv_package ).

        TRY.
            CALL METHOD cl_fp_wb_form=>('CREATE')
              EXPORTING
                i_name     = lv_name
                i_form     = li_form
                i_ordernum = iv_transport
                i_dark     = abap_true " > 740
              RECEIVING
                r_wb_form  = li_wb_object.
          CATCH cx_sy_dyn_call_error.
            li_wb_object = cl_fp_wb_form=>create(
              i_name     = lv_name
              i_form     = li_form
              i_ordernum = iv_transport ).
        ENDTRY.

        li_wb_object->save( ).
        li_wb_object->free( ).
      CATCH cx_fp_api INTO lx_fp_err.
        Lcx_abapgit_exception=>raise( |SFPF deserialization error: { lx_fp_err->get_text( ) }| ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE fpname.

    SELECT SINGLE name FROM fplayout
      INTO lv_name
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EFPFORM'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_xstr            TYPE xstring,
          li_document        TYPE REF TO if_ixml_document,
          li_node_collection TYPE REF TO if_ixml_node_collection,
          li_node_iter       TYPE REF TO if_ixml_node_iterator,
          li_node            TYPE REF TO if_ixml_node,
          li_node_new        TYPE REF TO if_ixml_node,
          li_node_parent     TYPE REF TO if_ixml_node.

    lv_xstr = form_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).

*   Clear CACHE_INFO
    li_node_collection = li_document->get_elements_by_tag_name_ns( 'CACHE_INFO' ).
    IF li_node_collection IS NOT INITIAL.
      li_node_iter = li_node_collection->create_iterator( ).
      DO.
        li_node = li_node_iter->get_next( ).
        IF li_node IS INITIAL.
          EXIT.
        ENDIF.
        li_node_new = li_document->create_element_ns( 'CACHE_INFO' ).
        li_node_parent = li_node->get_parent( ).
        li_node_parent->replace_child( new_child = li_node_new
                                       old_child = li_node ).
      ENDDO.
    ENDIF.

    fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SFPF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SFPI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sfpi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sfpi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SFPI implementation.
*"* method's implementations
*include methods.
  METHOD interface_to_xstring.

    DATA: li_fp_interface TYPE REF TO if_fp_interface,
          li_wb_interface TYPE REF TO if_fp_wb_interface.


    TRY.
        li_wb_interface = load( ).
        li_fp_interface ?= li_wb_interface->get_object( ).
        rv_xstr = cl_fp_helper=>convert_interface_to_xstring( li_fp_interface ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, interface_to_xstring' ).
    ENDTRY.

  ENDMETHOD.
  METHOD load.

    DATA: lv_name TYPE fpname.


    lv_name = ms_item-obj_name.

    TRY.
        ri_wb_interface = cl_fp_wb_interface=>load( lv_name ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, load' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lastuser FROM fpinterface
      INTO rv_user
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    IF rv_user IS INITIAL.
      SELECT SINGLE firstuser FROM fpinterface
        INTO rv_user
        WHERE name = ms_item-obj_name
        AND state = 'A'.
    ENDIF.
    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_name         TYPE fpname,
          lo_wb_interface TYPE REF TO cl_fp_wb_interface.


    lo_wb_interface ?= load( ).

    lv_name = ms_item-obj_name.

    TRY.
        lo_wb_interface->delete( lv_name ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, delete' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_xstr      TYPE xstring,
          lv_name      TYPE fpname,
          li_wb_object TYPE REF TO if_fp_wb_interface,
          li_interface TYPE REF TO if_fp_interface.


    lv_name = ms_item-obj_name.
    lv_xstr = cl_ixml_80_20=>render_to_xstring( io_xml->get_raw( ) ).

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    TRY.
        li_interface = cl_fp_helper=>convert_xstring_to_interface( lv_xstr ).
        tadir_insert( iv_package ).
        li_wb_object = cl_fp_wb_interface=>create( i_name      = lv_name
                                                   i_interface = li_interface ).
        li_wb_object->save( ).
        li_wb_object->free( ).
      CATCH cx_fp_api.
        Lcx_abapgit_exception=>raise( 'SFPI error, deserialize' ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE fpinterface-name.

    SELECT SINGLE name FROM fpinterface
      INTO lv_name
      WHERE name = ms_item-obj_name
      AND state = 'A'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = |{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EFPINTERFACE'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_xstr     TYPE xstring,
          li_document TYPE REF TO if_ixml_document.


    lv_xstr = interface_to_xstring( ).
    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xstr ).
    Lcl_abapgit_object_sfpf=>fix_oref( li_document ).
    io_xml->set_raw( li_document->get_root_element( ) ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SFPI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_W3HT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_w3ht=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_w3ht=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_W3HT implementation.
*"* method's implementations
*include methods.
  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_W3HT implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_W3MI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_w3mi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_w3mi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_W3MI implementation.
*"* method's implementations
*include methods.
  METHOD change_bdc_jump_data.

    DATA: ls_bdcdata LIKE LINE OF ct_bdcdata.

    ls_bdcdata-fnam = 'RADIO_HT'.
    ls_bdcdata-fval = ' '.
    APPEND ls_bdcdata TO ct_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RADIO_MI'.
    ls_bdcdata-fval = 'X'.
    APPEND ls_bdcdata TO ct_bdcdata.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_W3MI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SMIM <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_smim=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_smim=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SMIM implementation.
*"* method's implementations
*include methods.
  METHOD build_filename.

    CONCATENATE ms_item-obj_name ms_item-obj_type iv_filename
      INTO rv_filename SEPARATED BY '.'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.
  METHOD find_content.

    DATA: lv_filename TYPE string,
          lt_files    TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lt_files.


    lv_filename = get_filename( iv_url ).

    lv_filename = build_filename( lv_filename ).

    lt_files = Lif_abapgit_object~mo_files->get_files( ).

    READ TABLE lt_files ASSIGNING <ls_file>
        WITH KEY file
        COMPONENTS filename = lv_filename.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'SMIM, file not found' ).
    ENDIF.

    rv_content = <ls_file>-data.

  ENDMETHOD.
  METHOD get_filename.

    DATA: lv_lines   TYPE i,
          lt_strings TYPE TABLE OF string.


    SPLIT iv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO rv_filename.
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD get_url_for_io.

    DATA: ls_io       TYPE skwf_io,
          lv_url      TYPE skwf_url,
          ls_smimloio TYPE smimloio,
          lv_loio     TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    CLEAR ev_url.
    CLEAR ev_is_folder.

    SELECT SINGLE * FROM smimloio INTO ls_smimloio
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

    IF ls_smimloio-lo_class = wbmr_c_skwf_folder_class.
      ev_is_folder = abap_true.
      ls_io-objtype = skwfc_obtype_folder.
    ELSE.
      ls_io-objtype = skwfc_obtype_loio.
    ENDIF.
    ls_io-class = ls_smimloio-lo_class.
    ls_io-objid = ls_smimloio-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = ls_io
      IMPORTING
        url = lv_url.

    ev_url = lv_url.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_loio TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    SELECT SINGLE chng_user FROM smimloio INTO rv_user
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      SELECT SINGLE chng_user FROM smimphio INTO rv_user
        WHERE loio_id = lv_loio.                        "#EC CI_GENBUFF
      IF sy-subrc <> 0 OR rv_user IS INITIAL.
        rv_user = c_user_unknown.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_api TYPE REF TO if_mr_api,
          lv_url TYPE string.


    TRY.
        get_url_for_io( IMPORTING ev_url  = lv_url ).
      CATCH Lcx_abapgit_not_found.
        " Deleted already (maybe by "folder with children") but record deletion in transport
        corr_insert( iv_package ).
        RETURN.
    ENDTRY.

    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    li_api->delete(
      EXPORTING
        i_url              = lv_url
        i_delete_children  = abap_true
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        cancelled          = 3
        permission_failure = 4
        not_found          = 5
        OTHERS             = 6 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lv_content  TYPE xstring,
          lv_filename TYPE skwf_filnm,
          lv_io       TYPE sdok_docid,
          lv_class    TYPE smimloio-lo_class,
          ls_skwf_io  TYPE skwf_io,
          li_api      TYPE REF TO if_mr_api.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = ms_item-obj_name.

    io_xml->read( EXPORTING iv_name = 'URL'
                  CHANGING cg_data = lv_url ).
    io_xml->read( EXPORTING iv_name = 'FOLDER'
                  CHANGING cg_data = lv_folder ).
    io_xml->read( EXPORTING iv_name = 'CLASS'
                  CHANGING cg_data = lv_class ).

    ls_skwf_io-objid = lv_io.

    IF lv_folder = abap_true.
      li_api->create_folder(
        EXPORTING
          i_url              = lv_url
          i_language         = mv_language
          i_dev_package      = iv_package
          i_folder_loio      = ls_skwf_io
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          cancelled          = 3
          permission_failure = 4
          folder_exists      = 5
          OTHERS             = 6 ).
      IF sy-subrc <> 5 AND sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ELSE.
      lv_filename = get_filename( lv_url ).
      ls_skwf_io-class = lv_class.
      IF ls_skwf_io-class IS INITIAL.
        cl_wb_mime_repository=>determine_io_class(
          EXPORTING
            filename = lv_filename
          IMPORTING
            io_class = ls_skwf_io-class ).
        CONCATENATE ls_skwf_io-class '_L' INTO ls_skwf_io-class.
      ENDIF.

      lv_content = find_content( lv_url ).

      li_api->put(
        EXPORTING
          i_url                   = lv_url
          i_content               = lv_content
          i_dev_package           = iv_package
          i_new_loio              = ls_skwf_io
        EXCEPTIONS
          parameter_missing       = 1
          error_occured           = 2
          cancelled               = 3
          permission_failure      = 4
          data_inconsistency      = 5
          new_loio_already_exists = 6
          is_folder               = 7
          OTHERS                  = 8 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_loio TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    SELECT SINGLE loio_id FROM smimloio INTO lv_loio
      WHERE loio_id = lv_loio.                          "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lv_filename TYPE string,
          lv_class    TYPE smimloio-lo_class,
          ls_file     TYPE Lif_abapgit_git_definitions=>ty_file,
          lv_content  TYPE xstring,
          li_api      TYPE REF TO if_mr_api,
          lv_loio     TYPE sdok_docid.


    lv_loio = ms_item-obj_name.

    TRY.
        get_url_for_io(
          IMPORTING
            ev_url       = lv_url
            ev_is_folder = lv_folder ).
      CATCH Lcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    IF lv_folder = abap_false.
      li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
      li_api->get(
        EXPORTING
          i_url              = lv_url
        IMPORTING
          e_content          = lv_content
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4
          OTHERS             = 5 ).
      IF sy-subrc <> 0 AND sy-subrc <> 2 AND sy-subrc <> 3.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      lv_filename = get_filename( lv_url ).
      CLEAR ls_file.
      ls_file-filename = build_filename( lv_filename ).
      ls_file-path     = '/'.
      ls_file-data     = lv_content.
      Lif_abapgit_object~mo_files->add( ls_file ).

      SELECT SINGLE lo_class FROM smimloio INTO lv_class
        WHERE loio_id = lv_loio.                        "#EC CI_GENBUFF
    ENDIF.

    io_xml->add( iv_name = 'URL'
                 ig_data = lv_url ).
    io_xml->add( iv_name = 'FOLDER'
                 ig_data = lv_folder ).
    io_xml->add( iv_name = 'CLASS'
                 ig_data = lv_class ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SMIM implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SPLO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_splo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_splo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SPLO implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE chgname1 FROM tsp1d INTO rv_user
      WHERE papart = ms_item-obj_name.
    IF sy-subrc <> 0 OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DELETE FROM tsp1t WHERE papart = ms_item-obj_name. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM tsp1d WHERE papart = ms_item-obj_name.    "#EC CI_SUBRC
    DELETE FROM tsp0p WHERE pdpaper = ms_item-obj_name.   "#EC CI_SUBRC

    set_default_transport( iv_transport ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_tsp1t TYPE tsp1t,
          ls_tsp1d TYPE tsp1d,
          ls_tsp0p TYPE tsp0p.


    io_xml->read( EXPORTING iv_name = 'TSPLT'
                  CHANGING cg_data = ls_tsp1t ).
    io_xml->read( EXPORTING iv_name = 'TSPLD'
                  CHANGING cg_data = ls_tsp1d ).
    io_xml->read( EXPORTING iv_name = 'TSP0P'
                  CHANGING cg_data = ls_tsp0p ).

    MODIFY tsp1t FROM ls_tsp1t.                           "#EC CI_SUBRC
    MODIFY tsp1d FROM ls_tsp1d.                           "#EC CI_SUBRC
    MODIFY tsp0p FROM ls_tsp0p.                           "#EC CI_SUBRC

    set_default_transport( iv_transport ).

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_papart TYPE tsp1d-papart.


    SELECT SINGLE papart INTO lv_papart FROM tsp1d
      WHERE papart = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_tsp1t TYPE tsp1t,
          ls_tsp1d TYPE tsp1d,
          ls_tsp0p TYPE tsp0p.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tsp1t INTO ls_tsp1t
      WHERE papart = ms_item-obj_name
      AND spras = mv_language.            "#EC CI_GENBUFF "#EC CI_SUBRC
    SELECT SINGLE * FROM tsp1d INTO ls_tsp1d
      WHERE papart = ms_item-obj_name.                    "#EC CI_SUBRC
    SELECT SINGLE * FROM tsp0p INTO ls_tsp0p
      WHERE pdpaper = ms_item-obj_name.                   "#EC CI_SUBRC

    CLEAR: ls_tsp1d-chgname1,
           ls_tsp1d-chgtstmp1,
           ls_tsp1d-chgsaprel1,
           ls_tsp1d-chgsapsys1.

    io_xml->add( iv_name = 'TSPLT'
                 ig_data = ls_tsp1t ).
    io_xml->add( iv_name = 'TSPLD'
                 ig_data = ls_tsp1d ).
    io_xml->add( iv_name = 'TSP0P'
                 ig_data = ls_tsp0p ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SPLO implementation

*>>>>>>> ZCL_ABAPGIT_OO_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_oo_factory========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_oo_factory========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OO_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD make.
    IF gi_object_oriented_object IS BOUND.
      ri_object_oriented_object = gi_object_oriented_object.
      RETURN.
    ENDIF.
    IF iv_object_type = 'CLAS'.
      CREATE OBJECT ri_object_oriented_object TYPE Lcl_abapgit_oo_class.
    ELSEIF iv_object_type = 'INTF'.
      CREATE OBJECT ri_object_oriented_object TYPE Lcl_abapgit_oo_interface.
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OO_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SPRX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sprx=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sprx=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SPRX implementation.
*"* method's implementations
*include methods.
  METHOD check_sprx_tadir.

    DATA: lt_abap_keys TYPE prx_abapobjects,
          ls_abap_key  LIKE LINE OF lt_abap_keys,
          lx_error     TYPE REF TO cx_proxy_gen_error.

    ls_abap_key-object   = mv_object.
    ls_abap_key-obj_name = mv_obj_name.
    APPEND ls_abap_key TO lt_abap_keys.

    TRY.
        cl_proxy_utils=>check_sprx_tadir(
            objects = lt_abap_keys
            repair  = abap_true ).

      CATCH cx_proxy_gen_error INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    get_object_and_name(
      IMPORTING
        ev_object   = mv_object
        ev_obj_name = mv_obj_name ).

  ENDMETHOD.
  METHOD delta_handling.

    DATA: lo_proxy   TYPE REF TO cl_proxy,
          lt_delta   TYPE sprx_t_delta,
          ls_db_data TYPE sprx_db_data.

    "add Delta-Handling to avoid that single objects created without the dependent objects.
    "Thereby the dependent objects will be deleted
    TRY.
        lo_proxy = cl_proxy_fact=>load_by_abap_name(
                       object   = mv_object
                       obj_name = mv_obj_name ).


        lt_delta = lo_proxy->get_delta_all( ).

        ls_db_data = cl_proxy_db=>serialize(
                         proxy    = lo_proxy
                         inactive = abap_false
                         delta    = lt_delta ).

        et_sproxhdr_new = ls_db_data-sproxhdr.
        et_sproxdat_new = ls_db_data-sproxdat.

      CATCH cx_proxy_gen_error.
        "No delta for this object -> create

        ii_xml->read(
          EXPORTING
            iv_name = c_proxy-header
          CHANGING
            cg_data = et_sproxhdr_new ).

        IF et_sproxhdr_new IS INITIAL.
          Lcx_abapgit_exception=>raise( |SPRX - error deserialize: { ms_item-obj_name }| ).
        ENDIF.

        ii_xml->read(
          EXPORTING
            iv_name = c_proxy-data
          CHANGING
            cg_data = et_sproxdat_new ).

    ENDTRY.

  ENDMETHOD.
  METHOD get_object_and_name.

    ev_object   = ms_item-obj_name(4).
    ev_obj_name = ms_item-obj_name+4.

  ENDMETHOD.
  METHOD load_db.

* method cl_proxy_db=>load_by_abap_name does not exist in lower releases

    DATA: lt_packages TYPE prx_t_namespace_package,
          ls_package  LIKE LINE OF lt_packages,
          ls_hdr      TYPE prx_s_proxy_hdr,
          lv_package  TYPE tadir-devclass,
          lt_ids      TYPE prx_ids.

    cl_proxy_query=>get_hdr_by_abap_name(
      EXPORTING
        object   = mv_object
        obj_name = mv_obj_name
      IMPORTING
        hdr      = ls_hdr ).
    APPEND ls_hdr-id TO lt_ids.

    IF ls_hdr-gen_appl = 'WEBSERVICES'.
      cl_proxy_utils=>get_package(
        EXPORTING
          object   = mv_object
          obj_name = mv_obj_name
        RECEIVING
          rval     = lv_package
        EXCEPTIONS
          OTHERS   = 0 ).

      ls_package-namespace = ls_hdr-esr_nspce.
      ls_package-prefix    = ls_hdr-prefix.
      ls_package-package   = lv_package.
      APPEND ls_package TO lt_packages.
    ENDIF.

    rs_data = cl_proxy_db=>load(
      inactive               = abap_false
      ids                    = lt_ids
      generating_application = ls_hdr-gen_appl
      packages               = lt_packages ).

  ENDMETHOD.
  METHOD save.

    DATA:
      lt_sproxhdr_old  TYPE sprx_hdr_t,
      lt_sproxdat_old  TYPE sprx_dat_t,
      lt_sproxsvar_old TYPE sprx_svar_t,
      lt_sproxintf_old TYPE sprx_matchintf_t,
      lt_sproxsvar_new TYPE sprx_svar_t,
      lt_sproxintf_new TYPE sprx_matchintf_t.

    cl_proxy_data=>db_save(
        sproxhdr_old  = lt_sproxhdr_old
        sproxdat_old  = lt_sproxdat_old
        sproxsvar_old = lt_sproxsvar_old
        sproxintf_old = lt_sproxintf_old
        sproxhdr_new  = it_sproxhdr_new
        sproxdat_new  = it_sproxdat_new
        sproxsvar_new = lt_sproxsvar_new
        sproxintf_new = lt_sproxintf_new ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA lv_changed_by TYPE sproxhdr-changed_by.

    rv_user = c_user_unknown.

    SELECT SINGLE changed_by
      FROM sproxhdr
      INTO lv_changed_by
      WHERE object = mv_object
      AND obj_name = mv_obj_name
      AND inactive = abap_false.

    IF sy-subrc = 0 AND lv_changed_by IS NOT INITIAL.
      rv_user = lv_changed_by.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lv_object      TYPE sproxhdr-object,
      lv_obj_name    TYPE sproxhdr-obj_name,
      lv_transp_flag TYPE abap_bool,
      lv_return_code TYPE i,
      lt_log         TYPE sprx_log_t.

    IF iv_package(1) <> '$'.
      lv_transp_flag = abap_true.
    ENDIF.

    get_object_and_name(
      IMPORTING
        ev_object   = lv_object
        ev_obj_name = lv_obj_name ).

    TRY.
        CALL METHOD ('CL_PROXY_DATA')=>('DELETE_SINGLE_PROXY')
          EXPORTING
            object           = lv_object
            obj_name         = lv_obj_name
            i_transport      = lv_transp_flag
            suppress_dialogs = abap_true
          CHANGING
            c_return_code    = lv_return_code
            ct_log           = lt_log.
      CATCH cx_root.
        cl_proxy_data=>delete_single_proxy(
           EXPORTING
             object           = lv_object
             obj_name         = lv_obj_name
             i_transport      = lv_transp_flag
           CHANGING
             c_return_code    = lv_return_code
             ct_log           = lt_log ).
    ENDTRY.
    IF lv_return_code <> 0.
      Lcx_abapgit_exception=>raise( 'SPRX: Error from DELETE_SINGLE_PROXY' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lt_sproxhdr_new TYPE sprx_hdr_t,
          lt_sproxdat_new TYPE sprx_dat_t.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

    delta_handling(
      EXPORTING
        ii_xml = io_xml
      IMPORTING
        et_sproxhdr_new = lt_sproxhdr_new
        et_sproxdat_new = lt_sproxdat_new ).

    save(
      it_sproxhdr_new = lt_sproxhdr_new
      it_sproxdat_new = lt_sproxdat_new ).

    COMMIT WORK.

    check_sprx_tadir( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA:
      lv_status      TYPE prx_status,
      lv_status_text TYPE prx_status_t.

    cl_proxy_data=>db_get_status(
      EXPORTING
        object      = mv_object
        obj_name    = mv_obj_name
      IMPORTING
        status      = lv_status
        status_text = lv_status_text ).

    rv_bool = boolc( lv_status = if_proxy=>c_state_active ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true. "dummy implementation
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      ls_sprx_db_data TYPE sprx_db_data.

    FIELD-SYMBOLS:
      <ls_sproxheader> LIKE LINE OF ls_sprx_db_data-sproxhdr,
      <ls_sproxdat>    LIKE LINE OF ls_sprx_db_data-sproxdat.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_sprx_db_data = load_db( ).

    DELETE ls_sprx_db_data-sproxhdr WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxdat WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxsvar WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxpck WHERE object <> mv_object OR obj_name <> mv_obj_name.
    DELETE ls_sprx_db_data-sproxintf WHERE object <> mv_object OR obj_name <> mv_obj_name.

    IF lines( ls_sprx_db_data-sproxhdr ) <> 1.
      Lcx_abapgit_exception=>raise( |SPRX, no header found, { mv_object }, { mv_obj_name }| ).
    ENDIF.

    LOOP AT ls_sprx_db_data-sproxhdr ASSIGNING <ls_sproxheader>.

      CLEAR:
        <ls_sproxheader>-created_by,
        <ls_sproxheader>-created_on,
        <ls_sproxheader>-changed_by,
        <ls_sproxheader>-changed_on.

    ENDLOOP.

    LOOP AT ls_sprx_db_data-sproxdat ASSIGNING <ls_sproxdat>.

      CLEAR <ls_sproxdat>-warnings.

    ENDLOOP.

    io_xml->add(
        iv_name = c_proxy-header
        ig_data = ls_sprx_db_data-sproxhdr ).

    io_xml->add(
        iv_name = c_proxy-data
        ig_data = ls_sprx_db_data-sproxdat ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SPRX implementation

*>>>>>>> ZCL_ABAPGIT_PATH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_path==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_path==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_path==============ccau.




class LCL_ABAPGIT_PATH implementation.
*"* method's implementations
*include methods.
  METHOD change_dir.

    DATA: lv_last TYPE i,
          lv_temp TYPE string.

    lv_last = strlen( iv_cur_dir ) - 1.

    IF iv_cd = '' OR iv_cd = '.'. " No change
      rv_path = iv_cur_dir.
    ELSEIF iv_cd+0(1) = '/'.      " Absolute path
      rv_path = iv_cd.
    ELSEIF iv_cd = '..'.          " CD back
      IF iv_cur_dir = '/' OR iv_cur_dir = ''. " Back from root = root
        rv_path = iv_cur_dir.
      ELSE.
        lv_temp = reverse( iv_cur_dir ).
        IF lv_temp+0(1) = '/'.
          SHIFT lv_temp BY 1 PLACES LEFT.
        ENDIF.
        SHIFT lv_temp UP TO '/' LEFT.
        rv_path = reverse( lv_temp ).
      ENDIF.
    ELSEIF iv_cur_dir+lv_last(1) = '/'.  " Append cd to cur_dir separated by /
      rv_path = iv_cur_dir && iv_cd.
    ELSE.
      rv_path = iv_cur_dir && '/' && iv_cd.
    ENDIF.

    " TODO: improve logic and cases

  ENDMETHOD.
  METHOD get_filename_from_syspath.

    DATA: lv_split TYPE c LENGTH 1,
          lv_index TYPE i,
          lt_split TYPE TABLE OF string.

    " filename | c:\filename | /dir/filename | \\server\filename
    IF iv_path CA '/'.
      lv_split = '/'.
    ELSE.
      lv_split = '\'.
    ENDIF.

    SPLIT iv_path AT lv_split INTO TABLE lt_split.

    lv_index = lines( lt_split ).

    READ TABLE lt_split INDEX lv_index INTO rv_filename.

  ENDMETHOD.
  METHOD is_root.
    rv_yes = boolc( iv_path = '/' ).
  ENDMETHOD.
  METHOD is_subdir.

    DATA lv_len  TYPE i.
    DATA lv_last TYPE i.

    lv_len  = strlen( iv_parent ).
    lv_last = lv_len - 1.
    rv_yes  = boolc( strlen( iv_path ) > lv_len
                 AND iv_path+0(lv_len) = iv_parent
                 AND ( iv_parent+lv_last(1) = '/' OR iv_path+lv_len(1) = '/' ) ).

  ENDMETHOD.
  METHOD split_file_location.

    DATA: lv_cnt TYPE i,
          lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath
      MATCH COUNT lv_cnt
      MATCH LENGTH lv_len.

    IF lv_cnt > 0.
      ev_path     = iv_fullpath+0(lv_len).
      ev_filename = iv_fullpath+lv_len.
    ELSE.
      CLEAR ev_path.
      ev_filename = iv_fullpath.
    ENDIF.

    ev_filename = cl_http_utility=>unescape_url( escaped = ev_filename ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PATH implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SRFC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_srfc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_srfc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SRFC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    DATA li_srfc_persist TYPE REF TO if_wb_object_persist.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'Object type SRFC is not supported by this system' ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.

    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

        li_object_data->get_data( IMPORTING p_data = <lg_srfc_data> ).

        ASSIGN COMPONENT 'HEADER-CHANGEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0 AND <lg_any> IS NOT INITIAL.
          rv_user = <lg_any>.
        ELSE.
          rv_user = c_user_unknown.
        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_srfc_persist TYPE REF TO if_wb_object_persist,
          lx_error        TYPE REF TO cx_root.

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->delete( p_object_key = |{ ms_item-obj_name }|
                                 p_version    = 'A' ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_srfc_persist TYPE REF TO if_wb_object_persist,
          li_object_data  TYPE REF TO if_wb_object_data_model,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.

    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        ASSIGN COMPONENT 'HEADER-CREATEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-uname.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDON' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-datum.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDAT' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          <lg_any> = sy-uzeit.
        ENDIF.

        io_xml->read(
          EXPORTING
            iv_name = 'SRFC'
          CHANGING
            cg_data = <lg_srfc_data> ).

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').
        CREATE OBJECT li_object_data TYPE ('CL_UCONRFC_OBJECT_DATA').

        li_object_data->set_data( <lg_srfc_data> ).

        li_srfc_persist->save( li_object_data ).

        tadir_insert( iv_package ).

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist.

    TRY.
        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

      CATCH cx_root.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: li_object_data  TYPE REF TO if_wb_object_data_model,
          li_srfc_persist TYPE REF TO if_wb_object_persist,
          lr_srfc_data    TYPE REF TO data,
          lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_srfc_data> TYPE any,
                   <lg_any>       TYPE any.

    TRY.
        CREATE DATA lr_srfc_data TYPE ('UCONRFCSERV_COMPLETE').
        ASSIGN lr_srfc_data->* TO <lg_srfc_data>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT li_srfc_persist TYPE ('CL_UCONRFC_OBJECT_PERSIST').

        li_srfc_persist->get(
          EXPORTING
            p_object_key  = |{ ms_item-obj_name }|
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data ).

        li_object_data->get_data( IMPORTING p_data = <lg_srfc_data> ).

        ASSIGN COMPONENT 'HEADER-CREATEDBY' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDON' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

        ASSIGN COMPONENT 'HEADER-CREATEDAT' OF STRUCTURE <lg_srfc_data> TO <lg_any>.
        IF sy-subrc = 0.
          CLEAR <lg_any>.
        ENDIF.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add( iv_name = 'SRFC'
                 ig_data = <lg_srfc_data> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SRFC implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_BACKGROUND <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_backgroundccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_backgroundccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSIST_BACKGROUND implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mo_db = Lcl_abapgit_persistence_db=>get_instance( ).
  ENDMETHOD.
  METHOD delete.

    TRY.
        mo_db->read( iv_type  = Lcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
      CATCH Lcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    mo_db->delete( iv_type  = Lcl_abapgit_persistence_db=>c_type_background
                   iv_value = iv_key ).

  ENDMETHOD.
  METHOD exists.

    TRY.
        mo_db->read( iv_type  = Lcl_abapgit_persistence_db=>c_type_background
                     iv_value = iv_key ).
        rv_yes = abap_true.
      CATCH Lcx_abapgit_not_found.
        rv_yes = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD from_xml.
    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML iv_string
      RESULT data = rs_xml.
  ENDMETHOD.
  METHOD get_by_key.

    DATA: lt_list TYPE ty_background_keys.

    lt_list = list( ).

    READ TABLE lt_list WITH KEY key = iv_key INTO rs_data.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.
  METHOD list.

    DATA: lt_list TYPE Lif_abapgit_persistence=>ty_contents,
          ls_xml  TYPE ty_xml.

    FIELD-SYMBOLS: <ls_list>   LIKE LINE OF lt_list,
                   <ls_output> LIKE LINE OF rt_list.


    lt_list = mo_db->list_by_type( Lcl_abapgit_persistence_db=>c_type_background ).

    LOOP AT lt_list ASSIGNING <ls_list>.
      ls_xml = from_xml( <ls_list>-data_str ).

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_output>.
      MOVE-CORRESPONDING ls_xml TO <ls_output>.
      <ls_output>-key = <ls_list>-value.
    ENDLOOP.

  ENDMETHOD.
  METHOD modify.

    ASSERT NOT is_data-key IS INITIAL.

    mo_db->modify(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_background
      iv_value = is_data-key
      iv_data  = to_xml( is_data ) ).

  ENDMETHOD.
  METHOD to_xml.
    DATA: ls_xml TYPE ty_xml.

    MOVE-CORRESPONDING is_background TO ls_xml.

    CALL TRANSFORMATION id
      SOURCE data = ls_xml
      RESULT XML rv_string.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_BACKGROUND implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_PACKAGES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_packages==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_packages==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_persist_packages==ccau.


class LCL_ABAPGIT_PERSIST_PACKAGES implementation.
*"* method's implementations
*include methods.
  METHOD from_xml.

    DATA lo_input TYPE REF TO Lif_abapgit_xml_input.

    CREATE OBJECT lo_input TYPE Lcl_abapgit_xml_input EXPORTING iv_xml = iv_xml.

    lo_input->read(
      EXPORTING
        iv_name = Lcl_abapgit_persistence_db=>c_type_packages
      CHANGING
        cg_data = rt_packages ).

  ENDMETHOD.
  METHOD get_instance.

    IF go_persist IS NOT BOUND.
      CREATE OBJECT go_persist.
    ENDIF.
    ro_persist = go_persist.

  ENDMETHOD.
  METHOD init.

    TRY.
        " Might have changed in another session so always get latest
        mt_packages = from_xml( Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = Lcl_abapgit_persistence_db=>c_type_packages
          iv_value = '' ) ).
      CATCH Lcx_abapgit_exception Lcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD modify.

    DATA ls_package LIKE LINE OF mt_packages.

    FIELD-SYMBOLS <ls_package> LIKE LINE OF mt_packages.

    init( ).

    IF iv_component IS INITIAL AND iv_comp_posid IS INITIAL.
      DELETE mt_packages WHERE devclass = iv_package.
    ELSE.
      READ TABLE mt_packages ASSIGNING <ls_package> WITH TABLE KEY devclass = iv_package.
      IF sy-subrc = 0.
        <ls_package>-component  = iv_component.
        <ls_package>-comp_posid = iv_comp_posid.
      ELSE.
        ls_package-devclass   = iv_package.
        ls_package-component  = iv_component.
        ls_package-comp_posid = iv_comp_posid.
        INSERT ls_package INTO TABLE mt_packages.
      ENDIF.
    ENDIF.

    Lcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = Lcl_abapgit_persistence_db=>c_type_packages
      iv_value      = ''
      iv_data       = to_xml( mt_packages ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD read.

    init( ).

    READ TABLE mt_packages INTO rs_package WITH TABLE KEY devclass = iv_package.
    IF sy-subrc <> 0.
      rs_package-devclass = iv_package. " no component
    ENDIF.

  ENDMETHOD.
  METHOD to_xml.

    DATA li_output TYPE REF TO Lif_abapgit_xml_output.

    CREATE OBJECT li_output TYPE Lcl_abapgit_xml_output.

    li_output->add(
      iv_name = Lcl_abapgit_persistence_db=>c_type_packages
      ig_data = it_packages ).

    rv_xml = li_output->render( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_PACKAGES implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_SETTINGS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_settings==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_settings==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSIST_SETTINGS implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_persist_settings~modify.

    DATA: lv_settings      TYPE string,
          ls_user_settings TYPE Lif_abapgit_definitions=>ty_s_user_settings.


    lv_settings = io_settings->get_settings_xml( ).

    Lcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type       = Lcl_abapgit_persistence_db=>c_type_settings
      iv_value      = ''
      iv_data       = lv_settings ).

    ls_user_settings = io_settings->get_user_settings( ).

    Lcl_abapgit_persistence_user=>get_instance( )->set_settings( ls_user_settings ).

    " Settings have been modified: Update Buffered Settings
    IF mo_settings IS BOUND.
      mo_settings->set_xml_settings( lv_settings ).
      mo_settings->set_user_settings( ls_user_settings ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_settings~read.

    IF mo_settings IS BOUND.
      " Return Buffered Settings
      ro_settings = mo_settings.
      RETURN.
    ENDIF.

    " Settings have changed or have not yet been loaded
    CREATE OBJECT ro_settings.

    TRY.

        ro_settings->set_xml_settings(
          Lcl_abapgit_persistence_db=>get_instance( )->read(
            iv_type  = Lcl_abapgit_persistence_db=>c_type_settings
            iv_value = '' ) ).

        ro_settings->set_user_settings( Lcl_abapgit_persistence_user=>get_instance( )->get_settings( ) ).

      CATCH Lcx_abapgit_not_found Lcx_abapgit_exception.

        ro_settings->set_defaults( ).

    ENDTRY.

    mo_settings = ro_settings.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_SETTINGS implementation

*>>>>>>> ZCL_ABAPGIT_PROXY_AUTH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_proxy_auth========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_proxy_auth========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PROXY_AUTH implementation.
*"* method's implementations
*include methods.
  METHOD enter.

    Lcl_abapgit_password_dialog=>popup(
      EXPORTING
        iv_repo_url = 'Proxy Authentication'
      CHANGING
        cv_user     = gv_username
        cv_pass     = gv_password ).

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Proxy auth failed' ).
    ENDIF.

  ENDMETHOD.
  METHOD run.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      enter( ).
    ENDIF.

    ii_client->authenticate(
      proxy_authentication = abap_true
      username             = gv_username
      password             = gv_password ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PROXY_AUTH implementation

*>>>>>>> ZCL_ABAPGIT_PROXY_CONFIG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_proxy_config======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_proxy_config======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PROXY_CONFIG implementation.
*"* method's implementations
*include methods.
  METHOD bypass_proxy.

    DATA lt_proxy_bypass TYPE Lif_abapgit_definitions=>ty_range_proxy_bypass_url.

    lt_proxy_bypass = mo_settings->get_proxy_bypass( ).

    IF lt_proxy_bypass IS NOT INITIAL
    AND iv_repo_url IN lt_proxy_bypass.
      rv_bypass_proxy = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    mi_exit = Lcl_abapgit_exit=>get_instance( ).

  ENDMETHOD.
  METHOD get_proxy_authentication.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_auth = mo_settings->get_proxy_authentication( ).
    ENDIF.

    mi_exit->change_proxy_authentication(
      EXPORTING
        iv_repo_url            = iv_repo_url
      CHANGING
        cv_proxy_authentication = rv_auth ).

  ENDMETHOD.
  METHOD get_proxy_port.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_port = mo_settings->get_proxy_port( ).
    ENDIF.

    mi_exit->change_proxy_port(
      EXPORTING
        iv_repo_url  = iv_repo_url
      CHANGING
        cv_proxy_port = rv_port ).

    CONDENSE rv_port.

  ENDMETHOD.
  METHOD get_proxy_url.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_proxy_url = mo_settings->get_proxy_url( ).
    ENDIF.

    mi_exit->change_proxy_url(
      EXPORTING
        iv_repo_url = iv_repo_url
      CHANGING
        cv_proxy_url = rv_proxy_url ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PROXY_CONFIG implementation

*>>>>>>> ZCL_ABAPGIT_PR_ENUMERATOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_pr_enumerator=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_pr_enumerator=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PR_ENUMERATOR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    mv_repo_url = to_lower( iv_url ).
    TRY.
        mi_enum_provider = create_provider( mv_repo_url ).
      CATCH Lcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.
  METHOD create_provider.

    DATA li_agent TYPE REF TO Lif_abapgit_http_agent.
    DATA lv_user TYPE string.
    DATA lv_repo TYPE string.

    li_agent = Lcl_abapgit_factory=>get_http_agent( ).

    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)'
      IN iv_repo_url
      SUBMATCHES lv_user lv_repo.
    IF sy-subrc = 0.
      lv_repo = replace(
        val = lv_repo
        regex = '\.git$'
        with = '' ).
      CREATE OBJECT ri_provider TYPE Lcl_abapgit_pr_enum_github
        EXPORTING
          iv_user_and_repo = |{ lv_user }/{ lv_repo }|
          ii_http_agent    = li_agent.
    ELSE.
      Lcx_abapgit_exception=>raise( |PR enumeration is not supported for { iv_repo_url }| ).
    ENDIF.

    " TODO somewhen more providers

  ENDMETHOD.
  METHOD get_pulls.

    IF mi_enum_provider IS NOT BOUND.
      RETURN.
    ENDIF.

    rt_pulls = mi_enum_provider->list_pull_requests( ).

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance EXPORTING iv_url = iv_url.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PR_ENUMERATOR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_W3XX_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_w3xx_super=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_w3xx_super=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_W3XX_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( is_item = is_item
                        iv_language = iv_language ).
    ms_key-relid = ms_item-obj_type+2(2).
    ms_key-objid = ms_item-obj_name.
  ENDMETHOD.
  METHOD find_param.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF it_params.


    READ TABLE it_params ASSIGNING <ls_param> WITH KEY name = iv_name.
    IF sy-subrc > 0.
      Lcx_abapgit_exception=>raise( |W3xx: Cannot find { iv_name } for { ms_key-objid }| ).
    ENDIF.

    rv_value = <ls_param>-value.

  ENDMETHOD.
  METHOD get_ext.

    rv_ext = find_param( it_params = it_params
                         iv_name = c_param_names-fileext ).
    SHIFT rv_ext LEFT DELETING LEADING '.'.

  ENDMETHOD.
  METHOD normalize_params.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF ct_params.

    " Ensure filesize param exists
    READ TABLE ct_params ASSIGNING <ls_param> WITH KEY name = c_param_names-filesize.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ct_params ASSIGNING <ls_param>.
      <ls_param>-name  = c_param_names-filesize.
    ENDIF.

    LOOP AT ct_params ASSIGNING <ls_param>.
      <ls_param>-relid = ms_key-relid. " Ensure param key = object key
      <ls_param>-objid = ms_key-objid.
      IF <ls_param>-name = c_param_names-filesize. " Patch filesize = real file size
        <ls_param>-value = iv_size.
        CONDENSE <ls_param>-value.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD strip_params.

    FIELD-SYMBOLS <ls_param> LIKE LINE OF ct_params.

    " Remove path from filename
    find_param( it_params = ct_params
                iv_name = c_param_names-filename ). " Check exists
    READ TABLE ct_params ASSIGNING <ls_param> WITH KEY name = c_param_names-filename.
    <ls_param>-value = Lcl_abapgit_path=>get_filename_from_syspath( |{ <ls_param>-value }| ).

    " Clear id and object name
    LOOP AT ct_params ASSIGNING <ls_param>.
      CLEAR: <ls_param>-relid, <ls_param>-objid.
    ENDLOOP.

    " Clear version & filesize
    DELETE ct_params WHERE name = c_param_names-version.
    DELETE ct_params WHERE name = c_param_names-filesize.

    " Avoid diffs due to different order
    SORT ct_params.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE chname INTO rv_user
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key               = ms_key
      EXCEPTIONS
        wrong_object_type = 1
        delete_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot delete W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_DELETE_ALL'
      EXPORTING
        key          = ms_key
      EXCEPTIONS
        delete_error = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot delete W3xx params' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA lv_base64str TYPE string.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lv_size      TYPE i.


    io_xml->read( EXPORTING iv_name = 'TEXT'
                  CHANGING  cg_data = ms_key-text ).

    io_xml->read( EXPORTING iv_name = 'PARAMS'
                  CHANGING  cg_data = lt_w3params ).

    CASE io_xml->get_metadata( )-version.
      WHEN 'v1.0.0'.
        io_xml->read( EXPORTING iv_name = 'DATA'
                      CHANGING  cg_data = lv_base64str ).
        lv_xstring = cl_http_utility=>decode_x_base64( lv_base64str ).
      WHEN 'v2.0.0'.
        lv_xstring = Lif_abapgit_object~mo_files->read_raw( iv_extra = 'data'
                                                    iv_ext   = get_ext( lt_w3params ) ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'W3xx: Unknown serializer version' ).
    ENDCASE.

    CASE ms_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime.

        CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
          EXPORTING
            input_length  = lv_size
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_w3mime
            text_tab      = lt_w3html
          EXCEPTIONS
            failed        = 1.
        IF sy-subrc IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( 'Cannot update W3xx params' ).
        ENDIF.

        CLEAR lt_w3mime.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    " Update size of file based on actual data file size, prove param object name
    normalize_params( EXPORTING iv_size   = lv_size
                      CHANGING  ct_params = lt_w3params ).

    CALL FUNCTION 'WWWPARAMS_UPDATE'
      TABLES
        params       = lt_w3params
      EXCEPTIONS
        update_error = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot update W3xx params' ).
    ENDIF.

    ms_key-tdate    = sy-datum.
    ms_key-ttime    = sy-uzeit.
    ms_key-chname   = sy-uname.
    ms_key-devclass = iv_package.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        export_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot upload W3xx data' ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT SINGLE objid INTO ms_key-objid
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata         = get_metadata( ).
    rs_metadata-version = 'v2.0.0'. " Serialization v2, separate data file
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = |{ ms_item-obj_type+2(2) }{ ms_item-obj_name }|.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_WWW_HTML'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bdcdata TYPE bdcdata,
          lt_bdcdata TYPE ty_bdcdata.

    ls_bdcdata-program  = 'SAPMWWW0'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    change_bdc_jump_data( CHANGING ct_bdcdata = lt_bdcdata ).

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=CRO1'.
    APPEND ls_bdcdata TO lt_bdcdata.

    ls_bdcdata-program  = 'RSWWWSHW'.
    ls_bdcdata-dynpro   = '1000'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam     = 'SO_OBJID-LOW'.
    ls_bdcdata-fval     = ms_item-obj_name.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=ONLI'.
    APPEND ls_bdcdata TO lt_bdcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SMW0'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA lt_w3mime    TYPE STANDARD TABLE OF w3mime.
    DATA lt_w3html    TYPE STANDARD TABLE OF w3html.
    DATA lt_w3params  TYPE STANDARD TABLE OF wwwparams.
    DATA lv_xstring   TYPE xstring.
    DATA lv_size      TYPE i.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF ms_key
      FROM wwwdata
      WHERE relid = ms_key-relid
      AND objid = ms_key-objid
      AND srtf2 = 0.

    IF sy-subrc IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ms_key
      TABLES
        mime              = lt_w3mime
        html              = lt_w3html
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    CALL FUNCTION 'WWWPARAMS_READ_ALL'
      EXPORTING
        type             = ms_key-relid
        objid            = ms_key-objid
      TABLES
        params           = lt_w3params
      EXCEPTIONS
        entry_not_exists = 1.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot read W3xx data' ).
    ENDIF.

    lv_size = find_param( it_params = lt_w3params
                          iv_name = c_param_names-filesize ).
    " Clean params (remove version, filesize & clear filename from path)
    strip_params( CHANGING  ct_params = lt_w3params ).

    CASE ms_key-relid.
      WHEN 'MI'.
        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_size
          IMPORTING
            buffer       = lv_xstring
          TABLES
            binary_tab   = lt_w3mime
          EXCEPTIONS
            failed       = 1.
      WHEN 'HT'.
        CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
          IMPORTING
            buffer   = lv_xstring
          TABLES
            text_tab = lt_w3html
          EXCEPTIONS
            failed   = 1.
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise( 'Wrong W3xx type' ).
    ENDCASE.

    IF sy-subrc IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( 'Cannot convert W3xx to xstring' ).
    ENDIF.

    io_xml->add( iv_name = 'NAME'
                 ig_data = ms_key-objid ).

    io_xml->add( iv_name = 'TEXT'
                 ig_data = ms_key-text ).

    SORT lt_w3params.

    io_xml->add( iv_name = 'PARAMS'
                 ig_data = lt_w3params ).

    " Seriazation v2, separate data file. 'extra' added to prevent conflict with .xml
    Lif_abapgit_object~mo_files->add_raw( iv_data  = lv_xstring
                                  iv_extra = 'data'
                                  iv_ext   = get_ext( lt_w3params ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_W3XX_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_REPO_CS_MIGRATION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_cs_migration=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_cs_migration=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_CS_MIGRATION implementation.
*"* method's implementations
*include methods.
  METHOD clear_repo_metadata.

    DATA lo_repo_persistence TYPE REF TO Lcl_abapgit_persistence_repo.

    lo_repo_persistence ?= Lcl_abapgit_persist_factory=>get_repo( ).
    lo_repo_persistence->rewrite_repo_meta( iv_repo_key ).

  ENDMETHOD.
  METHOD convert_checksums.

    DATA lo_cs TYPE REF TO Lcl_abapgit_repo_checksums.
    DATA lv_xml TYPE Lif_abapgit_persistence=>ty_content-data_str.
    DATA:
      BEGIN OF ls_repo_extract,
        local_checksums TYPE Lif_abapgit_persistence=>ty_local_checksum_tt,
      END OF ls_repo_extract.

    lv_xml = Lcl_abapgit_persistence_db=>get_instance( )->read(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
      iv_value = iv_repo_key ).

    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_REPO_--29>' IN lv_xml WITH '<REPO>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_REPO_--29>' IN lv_xml WITH '</REPO>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT repo = ls_repo_extract.

    IF lines( ls_repo_extract-local_checksums ) = 0.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cs EXPORTING iv_repo_key = iv_repo_key.
    lo_cs->force_write( ls_repo_extract-local_checksums ).

  ENDMETHOD.
  METHOD get_unconverted_repo_ids.

    DATA lt_cs_ids TYPE ty_repo_ids.
    DATA lv_repo_id LIKE LINE OF rt_repo_ids.
    DATA lv_index TYPE i.

    SELECT value FROM (Lcl_abapgit_persistence_db=>c_tabname)
      INTO TABLE rt_repo_ids
      WHERE type = Lcl_abapgit_persistence_db=>c_type_repo.
    SELECT value FROM (Lcl_abapgit_persistence_db=>c_tabname)
      INTO TABLE lt_cs_ids
      WHERE type = Lcl_abapgit_persistence_db=>c_type_repo_csum.

    LOOP AT rt_repo_ids INTO lv_repo_id.
      lv_index = sy-tabix.
      READ TABLE lt_cs_ids TRANSPORTING NO FIELDS WITH KEY table_line = lv_repo_id.
      IF sy-subrc = 0. " Already converted
        DELETE rt_repo_ids INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD run.

    DATA lt_repo_ids TYPE ty_repo_ids.
    DATA lv_repo_id LIKE LINE OF lt_repo_ids.

    lt_repo_ids = get_unconverted_repo_ids( ).

    LOOP AT lt_repo_ids INTO lv_repo_id.
      convert_checksums( lv_repo_id ).
      clear_repo_metadata( lv_repo_id ).
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_CS_MIGRATION implementation

*>>>>>>> ZCL_ABAPGIT_REPO_FILTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_repo_filter=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_repo_filter=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_REPO_FILTER implementation.
*"* method's implementations
*include methods.
  METHOD apply.

    DATA: lt_filter TYPE SORTED TABLE OF Lif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name,
          lv_index  TYPE i.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir.

    filter_generated_tadir( CHANGING ct_tadir = ct_tadir ).

    IF lines( it_filter ) = 0.
      RETURN.
    ENDIF.

    lt_filter = it_filter.

* this is another loop at TADIR, but typically the filter is blank
    LOOP AT ct_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.
      READ TABLE lt_filter TRANSPORTING NO FIELDS WITH KEY object = <ls_tadir>-object
                                                           obj_name = <ls_tadir>-obj_name
                                                  BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE ct_tadir INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD apply_object_filter.
    DATA lr_file TYPE REF TO Lif_abapgit_git_definitions=>ty_file.
    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.
    DATA ls_tadir TYPE Lif_abapgit_definitions=>ty_tadir.
    DATA lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt.
    DATA lt_filter TYPE SORTED TABLE OF Lif_abapgit_definitions=>ty_tadir
                      WITH NON-UNIQUE KEY object obj_name.

    lt_filter = it_filter.

    LOOP AT ct_files REFERENCE INTO lr_file.
      IF lr_file->filename = Lif_abapgit_definitions=>c_dot_abapgit.
        CONTINUE.
      ENDIF.

      Lcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = lr_file->filename
          iv_path     = lr_file->path
          iv_devclass = iv_devclass
          io_dot      = io_dot
        IMPORTING
          es_item     = ls_item ).

      CLEAR lt_tadir.
      CLEAR ls_tadir.

      ls_tadir-object = ls_item-obj_type.
      ls_tadir-obj_name = ls_item-obj_name.
      ls_tadir-devclass = ls_item-devclass.

      INSERT ls_tadir INTO TABLE lt_tadir.

      filter_generated_tadir( CHANGING ct_tadir = lt_tadir ).

      IF lt_tadir IS INITIAL.
        DELETE ct_files.
        CONTINUE.
      ENDIF.

      READ TABLE lt_filter TRANSPORTING NO FIELDS
      WITH KEY object = ls_tadir-object
               obj_name = ls_tadir-obj_name
      BINARY SEARCH.

      IF sy-subrc <> 0.
        DELETE ct_files.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD filter_generated_tadir.

    DATA: ls_tadir     TYPE Lif_abapgit_definitions=>ty_tadir,
          ls_tadir_gen TYPE Lif_abapgit_definitions=>ty_tadir,
          lv_cd_object TYPE cdobjectcl,
          lt_cd_names  TYPE STANDARD TABLE OF cdnames,
          ls_cd_names  TYPE cdnames,
          lt_tcdrs     TYPE STANDARD TABLE OF tcdrs,
          ls_tcdrs     TYPE tcdrs.

    LOOP AT ct_tadir INTO ls_tadir WHERE pgmid = 'R3TR' AND object = 'CHDO'.
      CLEAR: lv_cd_object, lt_cd_names, ls_tadir_gen, lt_tcdrs, ls_tcdrs.

      lv_cd_object = ls_tadir-obj_name.

      CALL FUNCTION 'CDNAMES_GET'
        EXPORTING
          iv_object        = lv_cd_object
        TABLES
          it_names         = lt_cd_names
          it_tcdrs         = lt_tcdrs
        EXCEPTIONS
          object_space     = 1
          object_not_found = 2
          OTHERS           = 3.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_cd_names INTO ls_cd_names.
        DELETE ct_tadir WHERE pgmid = 'R3TR'
                          AND ( ( object = 'PROG'
                              AND ( obj_name = ls_cd_names-repnamec
                                 OR obj_name = ls_cd_names-repnamet
                                 OR obj_name = ls_cd_names-repnamefix
                                 OR obj_name = ls_cd_names-repnamevar ) )
                               OR object = 'FUGR' AND obj_name = ls_cd_names-fgrp ).
      ENDLOOP.

      LOOP AT lt_tcdrs INTO ls_tcdrs.
        DELETE ct_tadir WHERE pgmid = 'R3TR' AND object = 'TABL' AND obj_name = ls_tcdrs-tabname.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REPO_FILTER implementation

*>>>>>>> ZCL_ABAPGIT_REQUIREMENT_HELPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_requirement_helperccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_requirement_helperccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_requirement_helperccau.
CLASS SHRITEFUH64VYIPO5IWUYCIJCMQSFY DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get_sap_basis_component
      RETURNING
        VALUE(rs_result) TYPE cvers_sdu
      RAISING
        Lcx_abapgit_exception.

ENDCLASS.


CLASS SHRITEFUH64VYIPO5IWUYCIJCMQSFY IMPLEMENTATION.


  METHOD get_sap_basis_component.

    DATA:
      lt_installed TYPE STANDARD TABLE OF cvers_sdu.

    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ).
    ENDIF.

    READ TABLE lt_installed INTO rs_result
      WITH KEY component = `SAP_BASIS`.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Component SAP_BASIS not found| ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.





















class LCL_ABAPGIT_REQUIREMENT_HELPER implementation.
*"* method's implementations
*include methods.
  METHOD get_requirement_met_status.

    DATA: lt_installed TYPE STANDARD TABLE OF cvers_sdu.

    FIELD-SYMBOLS: <ls_requirement>    TYPE Lif_abapgit_dot_abapgit=>ty_requirement,
                   <ls_status>         TYPE ty_requirement_status,
                   <ls_installed_comp> TYPE cvers_sdu.


    CALL FUNCTION 'DELIVERY_GET_INSTALLED_COMPS'
      TABLES
        tt_comptab       = lt_installed
      EXCEPTIONS
        no_release_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from DELIVERY_GET_INSTALLED_COMPS { sy-subrc }| ).
    ENDIF.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO rt_status ASSIGNING <ls_status>.
      <ls_status>-component = <ls_requirement>-component.
      <ls_status>-required_release = <ls_requirement>-min_release.
      <ls_status>-required_patch = <ls_requirement>-min_patch.

      READ TABLE lt_installed WITH KEY component = <ls_requirement>-component
                              ASSIGNING <ls_installed_comp>.
      IF sy-subrc = 0.
        " Component is installed, requirement is met if the installed version is greater or equal
        " to the required one.
        <ls_status>-installed_release = <ls_installed_comp>-release.
        <ls_status>-installed_patch = <ls_installed_comp>-extrelease.
        <ls_status>-description = <ls_installed_comp>-desc_text.
        <ls_status>-met = is_version_greater_or_equal( <ls_status> ).
      ELSE.
        " Component is not installed at all
        <ls_status>-met = abap_false.
      ENDIF.

      UNASSIGN <ls_installed_comp>.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_requirements_met.

    DATA: lt_met_status TYPE ty_requirement_status_tt.

    lt_met_status = get_requirement_met_status( it_requirements ).

    READ TABLE lt_met_status TRANSPORTING NO FIELDS WITH KEY met = abap_false.
    IF sy-subrc = 0.
      rv_status = Lif_abapgit_definitions=>c_no.
    ELSE.
      rv_status = Lif_abapgit_definitions=>c_yes.
    ENDIF.

  ENDMETHOD.
  METHOD is_version_greater_or_equal.

    DATA:
      lv_installed_release TYPE n LENGTH 4,
      lv_installed_patch   TYPE n LENGTH 4,
      lv_required_release  TYPE n LENGTH 4,
      lv_required_patch    TYPE n LENGTH 4.

    TRY.
        MOVE EXACT: is_status-installed_release TO lv_installed_release,
                    is_status-installed_patch   TO lv_installed_patch,
                    is_status-required_release  TO lv_required_release,
                    is_status-required_patch    TO lv_required_patch.
      CATCH cx_sy_conversion_error.
        " Cannot compare by number, assume requirement not fullfilled (user can force install
        " anyways if this was an error)
        rv_true = abap_false.
        RETURN.
    ENDTRY.

    " Versions are comparable by number, compare release and if necessary patch level
    IF lv_installed_release > lv_required_release
        OR ( lv_installed_release = lv_required_release
         AND ( lv_required_patch = 0
            OR lv_installed_patch >= lv_required_patch ) ).

      rv_true = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD requirements_popup.

    DATA: lt_met_status TYPE ty_requirement_status_tt,
          lv_answer     TYPE c LENGTH 1.


    lt_met_status = get_requirement_met_status( it_requirements ).

    show_requirement_popup( lt_met_status ).

    lv_answer = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar      = 'Warning'
      iv_text_question = 'The project has unmet requirements. Do you want to continue?' ).

    IF lv_answer <> '1'.
      Lcx_abapgit_exception=>raise( 'Cancelling because of unmet requirements.' ).
    ENDIF.

  ENDMETHOD.
  METHOD show_requirement_popup.

    TYPES: BEGIN OF ty_color_line,
             color TYPE lvc_t_scol.
             INCLUDE TYPE ty_requirement_status.
    TYPES: END OF ty_color_line.

    TYPES: ty_color_tab TYPE STANDARD TABLE OF ty_color_line WITH DEFAULT KEY.

    DATA: lo_alv            TYPE REF TO cl_salv_table,
          lo_column         TYPE REF TO cl_salv_column,
          lo_columns        TYPE REF TO cl_salv_columns_table,
          lt_color_table    TYPE ty_color_tab,
          lt_color_negative TYPE lvc_t_scol,
          lt_color_positive TYPE lvc_t_scol,
          ls_color          TYPE lvc_s_scol,
          ls_position       TYPE Lif_abapgit_popups=>ty_popup_position,
          lx_ex             TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_line>        TYPE ty_color_line,
                   <ls_requirement> LIKE LINE OF it_requirements.


    ls_color-color-col = col_negative.
    APPEND ls_color TO lt_color_negative.

    ls_color-color-col = col_positive.
    APPEND ls_color TO lt_color_positive.

    CLEAR ls_color.

    LOOP AT it_requirements ASSIGNING <ls_requirement>.
      APPEND INITIAL LINE TO lt_color_table ASSIGNING <ls_line>.
      MOVE-CORRESPONDING <ls_requirement> TO <ls_line>.
    ENDLOOP.

    LOOP AT lt_color_table ASSIGNING <ls_line>.
      IF <ls_line>-met = abap_false.
        <ls_line>-color = lt_color_negative.
      ELSE.
        <ls_line>-color = lt_color_positive.
      ENDIF.
    ENDLOOP.
    UNASSIGN <ls_line>.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING t_table       = lt_color_table ).

        lo_columns = lo_alv->get_columns( ).
        lo_columns->get_column( 'MET' )->set_short_text( 'Met' ).
        lo_columns->set_color_column( 'COLOR' ).
        lo_columns->set_optimize( ).

        lo_column = lo_columns->get_column( 'REQUIRED_RELEASE' ).
        lo_column->set_short_text( 'Req. Rel.' ).

        lo_column = lo_columns->get_column( 'REQUIRED_PATCH' ).
        lo_column->set_short_text( 'Req. SP L.' ).

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 70
          iv_height = 10 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_alv->get_display_settings( )->set_list_header( 'Requirements' ).
        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found cx_salv_data_error INTO lx_ex.
        Lcx_abapgit_exception=>raise( lx_ex->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_REQUIREMENT_HELPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SUSH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sush=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sush=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SUSH implementation.
*"* method's implementations
*include methods.
  METHOD clear_metadata.

    DATA:
      BEGIN OF ls_empty_metadata,
        modifier  TYPE c LENGTH 12, " usob_sm-modifier
        moddate   TYPE d, " usob_sm-moddate,
        modtime   TYPE t, " usob_sm-modtime,
        srcsystem TYPE tadir-srcsystem,
        author    TYPE tadir-author,
        devclass  TYPE tadir-devclass,
      END OF ls_empty_metadata.

    FIELD-SYMBOLS:
      <ls_usobx>     TYPE any,
      <ls_usbot>     TYPE any,
      <ls_usobt_ext> TYPE any,
      <ls_usobx_ext> TYPE any.

    MOVE-CORRESPONDING ls_empty_metadata TO cs_data_head.

    LOOP AT ct_usobx ASSIGNING <ls_usobx>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobx>.
    ENDLOOP.

    LOOP AT ct_usobt ASSIGNING <ls_usbot>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usbot>.
    ENDLOOP.

    LOOP AT ct_usobt_ext ASSIGNING <ls_usobt_ext>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobt_ext>.
    ENDLOOP.

    LOOP AT ct_usobx_ext ASSIGNING <ls_usobx_ext>.
      MOVE-CORRESPONDING ls_empty_metadata TO <ls_usobx_ext>.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA: lr_data_head TYPE REF TO data.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').

      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |SUSH is not supported in your release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA ls_key TYPE usobkey.

    ls_key = ms_item-obj_name.

    SELECT SINGLE modifier FROM usob_sm INTO rv_user
      WHERE name = ls_key-name AND type = ls_key-type.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA:
      lo_su22 TYPE REF TO object,
      ls_key  TYPE        usobkey,
      lx_err  TYPE REF TO cx_static_check.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_su22 TYPE ('CL_SU22_ADT_OBJECT').

        CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~DELETE')
          EXPORTING
            iv_key     = ls_key
            iv_cleanup = abap_true.
      CATCH cx_static_check INTO lx_err.
        Lcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      ls_key            TYPE usobkey,
      lo_su22           TYPE REF TO object,
      lo_appl           TYPE REF TO object,
      lt_usobx          TYPE usobx_t,
      lt_usobt          TYPE usobt_t,
      lr_appl_head      TYPE REF TO data,
      lr_data_head      TYPE REF TO data,
      lr_data_usobx_ext TYPE REF TO data,
      lr_data_usobt_ext TYPE REF TO data,
      lx_err            TYPE REF TO cx_static_check,
      lv_text           TYPE string.

    FIELD-SYMBOLS: <ls_data_head>      TYPE any,
                   <ls_appl_head>      TYPE any,
                   <lt_data_usobx_ext> TYPE ANY TABLE,
                   <lt_data_usobt_ext> TYPE ANY TABLE,
                   <ls_devclass>       TYPE any.

    ASSERT NOT ms_item-obj_name IS INITIAL.

    TRY.
        CREATE DATA lr_data_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_data_head->* TO <ls_data_head>.

        CREATE DATA lr_data_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_data_usobx_ext->* TO <lt_data_usobx_ext>.

        CREATE DATA lr_data_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_data_usobt_ext->* TO <lt_data_usobt_ext>.

        "HEAD
        io_xml->read( EXPORTING iv_name = 'HEAD'
                      CHANGING  cg_data = <ls_data_head> ).

        "USOBX
        io_xml->read( EXPORTING iv_name = 'USOBX'
                      CHANGING  cg_data = lt_usobx ).

        "USOBT
        io_xml->read( EXPORTING iv_name = 'USOBT'
                      CHANGING  cg_data = lt_usobt ).

        "USOBX_EXT
        io_xml->read( EXPORTING iv_name = 'USOBX_EXT'
                      CHANGING  cg_data = <lt_data_usobx_ext> ).

        "USOBT_EXT
        io_xml->read( EXPORTING iv_name = 'USOBT_EXT'
                      CHANGING  cg_data = <lt_data_usobt_ext> ).

        CREATE OBJECT lo_su22
          TYPE ('CL_SU22_ADT_OBJECT').

        " check if lead application exists
        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~CHECK')
              EXPORTING
                id_mode = '02'
              CHANGING
                cs_head = <ls_data_head>.
          CATCH cx_static_check INTO lx_err.
            lv_text = |Lead application of object { ms_item-obj_name } does not exist|.
            Lcx_abapgit_exception=>raise( lv_text ).
        ENDTRY.

        MOVE-CORRESPONDING <ls_data_head> TO ls_key.
        CREATE DATA lr_appl_head TYPE ('CL_SU2X=>TS_HEAD').
        ASSIGN lr_appl_head->* TO <ls_appl_head>.

        CREATE OBJECT lo_appl TYPE ('CL_SU22_APPL').

        CALL METHOD lo_appl->('GET_DATA')
          EXPORTING
            is_key  = ls_key
          IMPORTING
            es_head = <ls_appl_head>.

        ASSIGN COMPONENT 'DEVCLASS' OF STRUCTURE <ls_appl_head> TO <ls_devclass>.
        IF <ls_devclass> <> iv_package.
          lv_text =
          |Lead application of object { ms_item-obj_name } does not exist in package { <ls_devclass> }|.
          Lcx_abapgit_exception=>raise( lv_text ).
        ENDIF.

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~UPDATE')
              EXPORTING
                is_head  = <ls_data_head>
                it_usobx = lt_usobx
                it_usobt = lt_usobt.
          CATCH cx_static_check INTO lx_err.
            Lcx_abapgit_exception=>raise_with_text( lx_err ).
        ENDTRY.

        corr_insert( iv_package ).

      CATCH cx_static_check INTO lx_err.
        Lcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    DATA: ls_usobhash TYPE usobhash.

    SELECT SINGLE * FROM usobhash INTO ls_usobhash "#EC CI_ALL_FIELDS_NEEDED
        WHERE name = ms_item-obj_name.                "#EC CI_SGLSELECT

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_USOBX'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      ls_key       TYPE usobkey,
      lo_su22      TYPE REF TO object,
      lt_usobx     TYPE usobx_t,
      lt_usobt     TYPE usobt_t,
      lr_head      TYPE REF TO data,
      lr_usobx_ext TYPE REF TO data,
      lr_usobt_ext TYPE REF TO data,
      lx_err       TYPE REF TO cx_static_check.


    FIELD-SYMBOLS: <ls_head>      TYPE any,
                   <lt_usobx_ext> TYPE ANY TABLE,
                   <lt_usobt_ext> TYPE ANY TABLE.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE DATA lr_head TYPE ('IF_SU22_ADT_OBJECT=>TS_SU2X_HEAD').
        ASSIGN lr_head->* TO <ls_head>.

        CREATE DATA lr_usobx_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_X').
        ASSIGN lr_usobx_ext->* TO <lt_usobx_ext>.

        CREATE DATA lr_usobt_ext TYPE ('IF_SU22_ADT_OBJECT=>TT_SU2X_T').
        ASSIGN lr_usobt_ext->* TO <lt_usobt_ext>.

        CREATE OBJECT lo_su22
          TYPE ('CL_SU22_ADT_OBJECT').

        TRY.
            CALL METHOD lo_su22->('IF_SU22_ADT_OBJECT~SELECT')
              EXPORTING
                iv_key       = ls_key
              IMPORTING
                es_head      = <ls_head>
                et_usobx     = lt_usobx
                et_usobt     = lt_usobt
                et_usobx_ext = <lt_usobx_ext>
                et_usobt_ext = <lt_usobt_ext>.
          CATCH cx_static_check INTO lx_err.
            Lcx_abapgit_exception=>raise_with_text( lx_err ).
        ENDTRY.

        clear_metadata(
          CHANGING
            cs_data_head = <ls_head>
            ct_usobx     = lt_usobx
            ct_usobt     = lt_usobt
            ct_usobx_ext = <lt_usobx_ext>
            ct_usobt_ext = <lt_usobt_ext> ).

        "HEAD
        io_xml->add( iv_name = 'HEAD'
                     ig_data = <ls_head> ).

        "USOBX
        io_xml->add( iv_name = 'USOBX'
                     ig_data = lt_usobx ).

        "USOBT
        io_xml->add( iv_name = 'USOBT'
                     ig_data = lt_usobt ).

        "USOBX_EXT
        io_xml->add( iv_name = 'USOBX_EXT'
                     ig_data = <lt_usobx_ext> ).

        "USOBT_EXT
        io_xml->add( iv_name = 'USOBT_EXT'
                     ig_data = <lt_usobt_ext> ).

      CATCH cx_static_check INTO lx_err.
        Lcx_abapgit_exception=>raise_with_text( lx_err ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SUSH implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_ABAP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_abap=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_abap=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_syntax_abap=======ccau.



*CLASS SHRITEFUH64VYIPO5IWUYCIJCNYSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCNYSFY.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYCIJCNYSFY DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*

*CLASS SHRITEFUH64VYIPO5IWUYCIJCN2SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCN2SFY.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYCIJCN2SFY definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYCIJCN2SFY IMPLEMENTATION
*----------------------------------------------------------------------*

class LCL_ABAPGIT_SYNTAX_ABAP implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

  ENDMETHOD.
  METHOD init_keywords.

    DATA: lv_keywords TYPE string,
          lt_keywords TYPE STANDARD TABLE OF string.

    lv_keywords =
     '&&|?TO|ABAP-SOURCE|ABBREVIATED|ABS|ABSTRACT|ACCEPT|ACCEPTING' &&
      '|ACCORDING|ACOS|ACTIVATION|ACTUAL|ADD|ADD-CORRESPONDING|ADJACENT|AFTER|ALIAS' &&
      '|ALIASES|ALIGN|ALL|ALLOCATE|ALPHA|ANALYSIS|ANALYZER|AND|ANY|APPEND|APPENDAGE' &&
      '|APPENDING|APPLICATION|ARCHIVE|AREA|ARITHMETIC|AS|ASCENDING|ASIN|ASPECT|ASSERT' &&
      '|ASSIGN|ASSIGNED|ASSIGNING|ASSOCIATION|ASYNCHRONOUS|AT|ATAN|ATTRIBUTES|AUTHORITY' &&
      '|AUTHORITY-CHECK|AVG|BACK|BACKGROUND|BACKUP|BACKWARD|BADI|BASE|BEFORE|BEGIN' &&
      '|BETWEEN|BIG|BINARY|BIT|BIT-AND|BIT-NOT|BIT-OR|BIT-XOR|BLACK|BLANK' &&
      '|BLANKS|BLOB|BLOCK|BLOCKS|BLUE|BOUND|BOUNDARIES|BOUNDS|BOXED|BREAK-POINT|BT' &&
      '|BUFFER|BY|BYPASSING|BYTE|BYTE-CA|BYTE-CN|BYTE-CO|BYTE-CS|BYTE-NA|BYTE-NS' &&
      '|BYTE-ORDER|C|CA|CALL|CALLING|CASE|CAST|CASTING|CATCH|CEIL|CENTER|CENTERED' &&
      '|CHAIN|CHAIN-INPUT|CHAIN-REQUEST|CHANGE|CHANGING|CHANNELS|CHARACTER|CHARLEN' &&
      '|CHAR-TO-HEX|CHECK|CHECKBOX|CI_|CIRCULAR|CLASS|CLASS-CODING|CLASS-DATA' &&
      '|CLASS-EVENTS|CLASS-METHODS|CLASS-POOL|CLEANUP|CLEAR|CLIENT|CLOB|CLOCK|CLOSE' &&
      '|CN|CO|COALESCE|CODE|CODING|COL_BACKGROUND|COL_GROUP|COL_HEADING|COL_KEY' &&
      '|COL_NEGATIVE|COL_NORMAL|COL_POSITIVE|COL_TOTAL|COLLECT|COLOR|COLUMN|COLUMNS' &&
      '|COMMENT|COMMENTS|COMMIT|COMMON|COMMUNICATION|COMPARING|COMPONENT|COMPONENTS' &&
      '|COMPRESSION|COMPUTE|CONCAT|CONCATENATE|COND|CONDENSE|CONDITION|CONNECT' &&
      '|CONNECTION|CONSTANTS|CONTEXT|CONTEXTS|CONTINUE|CONTROL|CONTROLS|CONV|CONVERSION' &&
      '|CONVERT|COPIES|COPY|CORRESPONDING|COS|COSH|COUNT|COUNTRY|COVER|CP|CPI|CREATE' &&
      '|CREATING|CRITICAL|CS|CURRENCY|CURRENCY_CONVERSION|CURRENT|CURSOR|CURSOR-SELECTION' &&
      '|CUSTOMER|CUSTOMER-FUNCTION|DANGEROUS|DATA|DATABASE|DATAINFO|DATASET|DATE' &&
      '|DAYLIGHT|DBMAXLEN|DD/MM/YY|DD/MM/YYYY|DDMMYY|DEALLOCATE|DECIMAL_SHIFT|DECIMALS' &&
      '|DECLARATIONS|DEEP|DEFAULT|DEFERRED|DEFINE|DEFINING|DEFINITION|DELETE|DELETING' &&
      '|DEMAND|DEPARTMENT|DESCENDING|DESCRIBE|DESTINATION|DETAIL|DIALOG|DIRECTORY' &&
      '|DISCONNECT|DISPLAY|DISPLAY-MODE|DISTANCE|DISTINCT|DIV|DIVIDE|DIVIDE-CORRESPONDING' &&
      '|DIVISION|DO|DUMMY|DUPLICATE|DUPLICATES|DURATION|DURING|DYNAMIC|DYNPRO' &&
      '|EDIT|EDITOR-CALL|ELSE|ELSEIF|EMPTY|ENABLED|ENABLING|ENCODING|END|ENDAT|ENDCASE' &&
      '|ENDCATCH|ENDCHAIN|ENDCLASS|ENDDO|ENDENHANCEMENT|END-ENHANCEMENT-SECTION' &&
      '|ENDEXEC|ENDFORM|ENDFUNCTION|ENDIAN|ENDIF|ENDING|ENDINTERFACE' &&
      '|END-LINES|ENDLOOP|ENDMETHOD|ENDMODULE|END-OF-DEFINITION|END-OF-FILE' &&
      '|END-OF-PAGE|END-OF-SELECTION|ENDON|ENDPROVIDE|ENDSELECT|ENDTRY|ENDWHILE' &&
      '|ENGINEERING|ENHANCEMENT|ENHANCEMENT-POINT|ENHANCEMENTS|ENHANCEMENT-SECTION' &&
      '|ENTRIES|ENTRY|ENVIRONMENT|EQ|EQUIV|ERRORMESSAGE|ERRORS|ESCAPE|ESCAPING' &&
      '|EVENT|EVENTS|EXACT|EXCEPT|EXCEPTION|EXCEPTIONS|EXCEPTION-TABLE|EXCLUDE|EXCLUDING' &&
      '|EXEC|EXECUTE|EXISTS|EXIT|EXIT-COMMAND|EXP|EXPAND|EXPANDING|EXPIRATION|EXPLICIT' &&
      '|EXPONENT|EXPORT|EXPORTING|EXTEND|EXTENDED|EXTENSION|EXTRACT|FAIL|FETCH|FIELD' &&
      '|FIELD-GROUPS|FIELDS|FIELD-SYMBOL|FIELD-SYMBOLS|FILE|FILTER|FILTERS|FILTER-TABLE' &&
      '|FINAL|FIND|FIRST|FIRST-LINE|FIXED-POINT|FKEQ|FKGE|FLOOR|FLUSH|FONT|FOR|FORM' &&
      '|FORMAT|FORWARD|FOUND|FRAC|FRAME|FRAMES|FREE|FRIENDS|FROM|FUNCTION|FUNCTIONALITY' &&
      '|FUNCTION-POOL|FURTHER|GAPS|GE|GENERATE|GET|GIVING|GKEQ|GKGE|GLOBAL|GRANT' &&
      '|GREEN|GROUP|GROUPS|GT|HANDLE|HANDLER|HARMLESS|HASHED|HAVING|HDB|HEADER|HEADERS' &&
      '|HEADING|HEAD-LINES|HELP-ID|HELP-REQUEST|HIDE|HIGH|HINT|HOLD|HOTSPOT|I|ICON|ID' &&
      '|IDENTIFICATION|IDENTIFIER|IDS|IF|IGNORE|IGNORING|IMMEDIATELY|IMPLEMENTATION' &&
      '|IMPLEMENTATIONS|IMPLEMENTED|IMPLICIT|IMPORT|IMPORTING|IN|INACTIVE|INCL|INCLUDE' &&
      '|INCLUDES|INCLUDING|INCREMENT|INDEX|INDEX-LINE|INFOTYPES|INHERITING|INIT|INITIAL' &&
      '|INITIALIZATION|INNER|INOUT|INPUT|INSERT|INSTANCES|INTENSIFIED|INTERFACE' &&
      '|INTERFACE-POOL|INTERFACES|INTERNAL|INTERVALS|INTO|INVERSE|INVERTED-DATE|IS' &&
      '|ISO|JOB|JOIN|KEEP|KEEPING|KERNEL|KEY|KEYS|KEYWORDS|KIND' &&
      '|LANGUAGE|LAST|LATE|LAYOUT|LE|LEADING|LEAVE|LEFT|LEFT-JUSTIFIED|LEFTPLUS' &&
      '|LEFTSPACE|LEGACY|LENGTH|LET|LEVEL|LEVELS|LIKE|LINE|LINE-COUNT|LINEFEED' &&
      '|LINES|LINE-SELECTION|LINE-SIZE|LIST|LISTBOX|LIST-PROCESSING|LITTLE|LLANG' &&
      '|LOAD|LOAD-OF-PROGRAM|LOB|LOCAL|LOCALE|LOCATOR|LOG|LOG10|LOGFILE|LOGICAL' &&
      '|LOG-POINT|LONG|LOOP|LOW|LOWER|LPAD|LPI|LT|M|MAIL|MAIN|MAJOR-ID|MAPPING|MARGIN' &&
      '|MARK|MASK|MATCH|MATCHCODE|MAX|MAXIMUM|MEDIUM|MEMBERS|MEMORY|MESH|MESSAGE' &&
      '|MESSAGE-ID|MESSAGES|MESSAGING|METHOD|METHODS|MIN|MINIMUM|MINOR-ID|MM/DD/YY' &&
      '|MM/DD/YYYY|MMDDYY|MOD|MODE|MODIF|MODIFIER|MODIFY|MODULE|MOVE|MOVE-CORRESPONDING' &&
      '|MULTIPLY|MULTIPLY-CORRESPONDING|NA|NAME|NAMETAB|NATIVE|NB|NE|NESTED|NESTING' &&
      '|NEW|NEW-LINE|NEW-PAGE|NEW-SECTION|NEXT|NO|NODE|NODES|NO-DISPLAY' &&
      '|NO-EXTENSION|NO-GAP|NO-GAPS|NO-GROUPING|NO-HEADING|NON-UNICODE|NON-UNIQUE' &&
      '|NO-SCROLLING|NO-SIGN|NOT|NO-TITLE|NO-TOPOFPAGE|NO-ZERO|NP|NS|NULL|NUMBER' &&
      '|NUMOFCHAR|O|OBJECT|OBJECTS|OBLIGATORY|OCCURRENCE|OCCURRENCES|OCCURS|OF|OFF' &&
      '|OFFSET|OLE|ON|ONLY|OPEN|OPTION|OPTIONAL|OPTIONS|OR|ORDER|OTHER|OTHERS|OUT' &&
      '|OUTER|OUTPUT|OUTPUT-LENGTH|OVERFLOW|OVERLAY|PACK|PACKAGE|PAD|PADDING|PAGE' &&
      '|PAGES|PARAMETER|PARAMETERS|PARAMETER-TABLE|PART|PARTIALLY|PATTERN|PERCENTAGE' &&
      '|PERFORM|PERFORMING|PERSON|PF1|PF2|PF3|PF4|PF5|PF6|PF7|PF8|PF9|PF10|PF11|PF12' &&
      '|PF13|PF14|PF15|PF-STATUS|PINK|PLACES|POOL|POS_HIGH|POS_LOW' &&
      '|POSITION|PRAGMAS|PRECOMPILED|PREFERRED|PRESERVING|PRIMARY|PRINT|PRINT-CONTROL' &&
      '|PRIORITY|PRIVATE|PROCEDURE|PROCESS|PROGRAM|PROPERTY|PROTECTED|PROVIDE|PUBLIC' &&
      '|PUSHBUTTON|PUT|QUEUE-ONLY|QUICKINFO|RADIOBUTTON|RAISE|RAISING|RANGE|RANGES' &&
      '|RAW|READ|READER|READ-ONLY|RECEIVE|RECEIVED|RECEIVER|RECEIVING|RED|REDEFINITION' &&
      '|REDUCE|REDUCED|REF|REFERENCE|REFRESH|REGEX|REJECT|REMOTE|RENAMING|REPLACE' &&
      '|REPLACEMENT|REPLACING|REPORT|REQUEST|REQUESTED|RESERVE|RESET|RESOLUTION' &&
      '|RESPECTING|RESPONSIBLE|RESULT|RESULTS|RESUMABLE|RESUME|RETRY|RETURN|RETURNCODE' &&
      '|RETURNING|RIGHT|RIGHT-JUSTIFIED|RIGHTPLUS|RIGHTSPACE|RISK|RMC_COMMUNICATION_FAILURE' &&
      '|RMC_INVALID_STATUS|RMC_SYSTEM_FAILURE|ROLE|ROLLBACK|ROUND|ROWS|RUN|SAP' &&
      '|SAP-SPOOL|SAVING|SCALE_PRESERVING|SCALE_PRESERVING_SCIENTIFIC|SCAN|SCIENTIFIC' &&
      '|SCIENTIFIC_WITH_LEADING_ZERO|SCREEN|SCROLL|SCROLL-BOUNDARY|SCROLLING|SEARCH' &&
      '|SECONDARY|SECONDS|SECTION|SELECT|SELECTION|SELECTIONS|SELECTION-SCREEN|SELECTION-SET' &&
      '|SELECTION-SETS|SELECTION-TABLE|SELECT-OPTIONS|SEND|SEPARATE|SEPARATED|SET' &&
      '|SHARED|SHIFT|SHORT|SHORTDUMP-ID|SIGN|SIGN_AS_POSTFIX|SIMPLE|SIN|SINGLE|SINH|SIZE' &&
      '|SKIP|SKIPPING|SMART|SOME|SORT|SORTABLE|SORTED|SOURCE|SPACE|SPECIFIED|SPLIT|SPOOL' &&
      '|SPOTS|SQL|SQLSCRIPT|SQRT|STABLE|STAMP|STANDARD|STARTING|START-OF-SELECTION|STATE' &&
      '|STATEMENT|STATEMENTS|STATIC|STATICS|STATUSINFO|STEP-LOOP|STOP|STRLEN|STRUCTURE' &&
      '|STRUCTURES|STYLE|SUBKEY|SUBMATCHES|SUBMIT|SUBROUTINE|SUBSCREEN|SUBSTRING|SUBTRACT' &&
      '|SUBTRACT-CORRESPONDING|SUFFIX|SUM|SUMMARY|SUMMING|SUPPLIED|SUPPLY|SUPPRESS|SWITCH' &&
      '|SWITCHSTATES|SYMBOL|SYNCPOINTS|SYNTAX|SYNTAX-CHECK|SYNTAX-TRACE' &&
      '|SYSTEM-CALL|SYSTEM-EXCEPTIONS|SYSTEM-EXIT|TAB|TABBED|TABLE|TABLES|TABLEVIEW|TABSTRIP' &&
      '|TAN|TANH|TARGET|TASK|TASKS|TEST|TESTING|TEXT|TEXTPOOL|THEN|THROW|TIME|TIMES|TIMESTAMP' &&
      '|TIMEZONE|TITLE|TITLEBAR|TITLE-LINES|TO|TOKENIZATION|TOKENS|TOP-LINES|TOP-OF-PAGE' &&
      '|TRACE-FILE|TRACE-TABLE|TRAILING|TRANSACTION|TRANSFER|TRANSFORMATION|TRANSLATE' &&
      '|TRANSPORTING|TRMAC|TRUNC|TRUNCATE|TRUNCATION|TRY|TYPE|TYPE-POOL|TYPE-POOLS|TYPES' &&
      '|ULINE|UNASSIGN|UNDER|UNICODE|UNION|UNIQUE|UNIT|UNIT_CONVERSION|UNIX|UNPACK|UNTIL' &&
      '|UNWIND|UP|UPDATE|UPPER|USER|USER-COMMAND|USING|UTF-8|VALID|VALUE|VALUE-REQUEST|VALUES' &&
      '|VARY|VARYING|VERIFICATION-MESSAGE|VERSION|VIA|VIEW|VISIBLE|WAIT|WARNING|WHEN|WHENEVER' &&
      '|WHERE|WHILE|WIDTH|WINDOW|WINDOWS|WITH|WITH-HEADING|WITHOUT|WITH-TITLE|WORD|WORK' &&
      '|WRITE|WRITER|X|XML|XSD|XSTRLEN|YELLOW|YES|YYMMDD|Z|ZERO|ZONE' &&
      '|BINTOHEX|CHAR|CLNT|CONCAT_WITH_SPACE|CURR|DATS|DATS_ADD_DAYS|DATS_ADD_MONTHS' &&
      '|DATS_DAYS_BETWEEN|DATS_IS_VALID|DEC|END-OF-EDITING|END-TEST-INJECTION|END-TEST-SEAM' &&
      '|ENDWITH|ENUM|HEXTOBIN|INSTANCE|INSTR|LANG|LTRIM|NUMC|PUSH' &&
      '|QUAN|RETURNS|RPAD|RTRIM|SSTRING|START-OF-EDITING|TEST-INJECTION|TEST-SEAM|TIMS' &&
      '|TIMS_IS_VALID|TSTMP_ADD_SECONDS|TSTMP_CURRENT_UTCTIMESTAMP|TSTMP_IS_VALID' &&
      '|TSTMP_SECONDS_BETWEEN|B|D|DECFLOAT16|DECFLOAT34|F|INT8|N|P|S|STRING|T|UTCLONG|XSTRING' &&
      '|ABAP_BOOL|ACCP|CUKY|DF16_DEC|DF16_RAW|DF34_DEC|DF34_RAW|FLTP' &&
      '|INT1|INT2|INT4|LCHR|LRAW|RAWSTRING|DF16_SCL|DF34_SCL' &&
      '|PREC|VARC|CLIKE|CSEQUENCE|DECFLOAT|NUMERIC|XSEQUENCE|ME|SYST|SY' &&
      '|BIT-SET|BOOLC|BOOLX|CHAR_OFF|CMAX|CMIN|CONCAT_LINES_OF|CONTAINS|CONTAINS_ANY_NOT_OF' &&
      '|CONTAINS_ANY_OF|COUNT_ANY_NOT_OF|COUNT_ANY_OF|FIND_ANY_NOT_OF|FIND_ANY_OF|FIND_END' &&
      '|FROM_MIXED|IPOW|LINE_EXISTS|LINE_INDEX|MATCHES|NMAX|NMIN|REPEAT|RESCALE|REVERSE' &&
      '|SEGMENT|SHIFT_LEFT|SHIFT_RIGHT|SUBSTRING_AFTER|SUBSTRING_BEFORE|SUBSTRING_FROM|SUBSTRING_TO' &&
      '|TO_LOWER|TO_MIXED|TO_UPPER|UTCLONG_ADD|UTCLONG_CURRENT|UTCLONG_DIFF|XSDBOOL'.


    SPLIT lv_keywords AT '|' INTO TABLE lt_keywords.
    " remove duplicates to avoid dumps when converting to a hash table
    SORT lt_keywords BY table_line ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_keywords.
    gt_keywords = lt_keywords. " Hash table

  ENDMETHOD.
  METHOD is_keyword.

    DATA lv_str TYPE string.

    lv_str = to_upper( iv_chunk ).
    READ TABLE gt_keywords WITH KEY table_line = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_index      TYPE sy-tabix,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    SORT ct_matches BY offset.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <ls_match>.
      lv_index = sy-tabix.

      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        DELETE ct_matches INDEX lv_index.
        CONTINUE.
      ENDIF.

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          IF <ls_match>-offset > 0
              AND substring( val = iv_line
                             off = ( <ls_match>-offset - 1 )
                             len = 1 ) CA '-<'.
            " Delete match if keyword is part of structure or field symbol
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

        WHEN c_token-comment.
          <ls_match>-length = lv_line_len - <ls_match>-offset.
          DELETE ct_matches FROM lv_index + 1.
          CONTINUE.

        WHEN c_token-text.
          <ls_match>-text_tag = substring( val = iv_line
                                        off = <ls_match>-offset
                                        len = <ls_match>-length ).
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ELSEIF <ls_prev>-text_tag = '}' AND <ls_match>-text_tag = '{'.
              <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset - 1.  " Shift } out of scope
              <ls_prev>-offset = <ls_prev>-offset + 1.                   " Shift { out of scope
              CLEAR lv_prev_token.
            ELSEIF <ls_match>-text_tag = '{'.
              <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ELSEIF <ls_prev>-text_tag = '}'.
              <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset.
              <ls_prev>-offset = <ls_prev>-offset + 1.                   " Shift } out of scope
              CLEAR lv_prev_token.
            ENDIF.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        DELETE rt_matches INDEX lv_index.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_ABAP implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_CSS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_css========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_css========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_CSS implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    add_rule( iv_regex = c_regex-selectors
              iv_token = c_token-selectors
              iv_style = c_css-selectors ).

    add_rule( iv_regex = c_regex-units
              iv_token = c_token-units
              iv_style = c_css-units ).

    " Styles for keywords
    add_rule( iv_regex = ''
              iv_token = c_token-html
              iv_style = c_css-html ).

    add_rule( iv_regex = ''
              iv_token = c_token-properties
              iv_style = c_css-properties ).

    add_rule( iv_regex = ''
              iv_token = c_token-values
              iv_style = c_css-values ).

    add_rule( iv_regex = ''
              iv_token = c_token-functions
              iv_style = c_css-functions ).

    add_rule( iv_regex = ''
              iv_token = c_token-colors
              iv_style = c_css-colors ).

    add_rule( iv_regex = ''
              iv_token = c_token-extensions
              iv_style = c_css-extensions ).

    add_rule( iv_regex = ''
              iv_token = c_token-at_rules
              iv_style = c_css-at_rules ).

  ENDMETHOD.
  METHOD init_keywords.

    DATA: lv_keywords TYPE string.

    CLEAR gt_keywords.

    " 1) CSS Properties
    lv_keywords =
    'align-content|align-items|align-self|animation|animation-delay|animation-direction|animation-duration|' &&
    'animation-fill-mode|animation-iteration-count|animation-name|animation-play-state|animation-timing-function|' &&
    'backface-visibility|background|background-attachment|background-blend-mode|background-clip|background-color|' &&
    'background-image|background-origin|background-position|background-repeat|background-size|border|' &&
    'border-bottom|border-bottom-color|border-bottom-left-radius|border-bottom-right-radius|border-bottom-style|' &&
    'border-bottom-width|border-collapse|border-color|border-image|border-image-outset|border-image-repeat|' &&
    'border-image-slice|border-image-source|border-image-width|border-left|border-left-color|border-left-style|' &&
    'border-left-width|border-radius|border-right|border-right-color|border-right-style|border-right-width|' &&
    'border-spacing|border-style|border-top|border-top-color|border-top-left-radius|border-top-right-radius|' &&
    'border-top-style|border-top-width|border-width|box-decoration-break|box-shadow|box-sizing|caption-side|' &&
    'caret-color|clear|clip|color|column-count|column-fill|column-gap|column-rule|column-rule-color|' &&
    'column-rule-style|column-rule-width|column-span|column-width|columns|content|counter-increment|' &&
    'counter-reset|cursor|direction|display|empty-cells|filter|flex|flex-basis|flex-direction|flex-flow|' &&
    'flex-grow|flex-shrink|flex-wrap|float|font|font-family|font-kerning|font-size|font-size-adjust|' &&
    'font-stretch|font-style|font-variant|font-weight|grid|grid-area|grid-auto-columns|grid-auto-flow|' &&
    'grid-auto-rows|grid-column|grid-column-end|grid-column-gap|grid-column-start|grid-gap|grid-row|' &&
    'grid-row-end|grid-row-gap|grid-row-start|grid-template|grid-template-areas|grid-template-columns|' &&
    'grid-template-rows|hanging-punctuation|height|hyphens|isolation|justify-content|' &&
    'letter-spacing|line-height|list-style|list-style-image|list-style-position|list-style-type|margin|' &&
    'margin-bottom|margin-left|margin-right|margin-top|max-height|max-width|media|min-height|min-width|' &&
    'mix-blend-mode|object-fit|object-position|opacity|order|outline|outline-color|outline-offset|' &&
    'outline-style|outline-width|overflow|overflow-x|overflow-y|padding|padding-bottom|padding-left|' &&
    'padding-right|padding-top|page-break-after|page-break-before|page-break-inside|perspective|' &&
    'perspective-origin|pointer-events|position|quotes|resize|scroll-behavior|tab-size|table-layout|' &&
    'text-align|text-align-last|text-decoration|text-decoration-color|text-decoration-line|' &&
    'text-decoration-style|text-indent|text-justify|text-overflow|text-rendering|text-shadow|text-transform|' &&
    'transform|transform-origin|transform-style|transition|transition-delay|transition-duration|' &&
    'transition-property|transition-timing-function|unicode-bidi|user-select|vertical-align|visibility|' &&
    'white-space|width|word-break|word-spacing|word-wrap|writing-mode|z-index'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-properties ).

    " 2) CSS Values
    lv_keywords =
    'absolute|all|auto|block|bold|border-box|both|bottom|center|counter|cover|dashed|fixed|hidden|important|' &&
    'inherit|initial|inline-block|italic|left|max-content|middle|min-content|no-repeat|none|normal|pointer|' &&
    'relative|rem|right|solid|table-cell|text|top|transparent|underline|url'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-values ).

    " 3) CSS Selectors
    lv_keywords =
    ':active|::after|::before|:checked|:disabled|:empty|:enabled|:first-child|::first-letter|::first-line|' &&
    ':first-of-type|:focus|:hover|:lang|:last-child|:last-of-type|:link|:not|:nth-child|:nth-last-child|' &&
    ':nth-last-of-type|:nth-of-type|:only-child|:only-of-type|:root|:target|:visited'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-selectors ).

    " 4) CSS Functions
    lv_keywords =
    'attr|calc|cubic-bezier|hsl|hsla|linear-gradient|radial-gradient|repeating-linear-gradient|' &&
    'repeating-radial-gradient|rgb|rgba|rotate|scale|translateX|translateY|var'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-functions ).

    " 5) CSS Colors
    lv_keywords =
    '#|aliceblue|antiquewhite|aqua|aquamarine|azure|beige|bisque|black|blanchedalmond|blue|blueviolet|brown|' &&
    'burlywood|cadetblue|chartreuse|chocolate|coral|cornflowerblue|cornsilk|crimson|cyan|darkblue|darkcyan|' &&
    'darkgoldenrod|darkgray|darkgreen|darkgrey|darkkhaki|darkmagenta|darkolivegreen|darkorange|darkorchid|' &&
    'darkred|darksalmon|darkseagreen|darkslateblue|darkslategray|darkslategrey|darkturquoise|darkviolet|' &&
    'deeppink|deepskyblue|dimgray|dimgrey|dodgerblue|firebrick|floralwhite|forestgreen|fuchsia|gainsboro|' &&
    'ghostwhite|gold|goldenrod|gray|green|greenyellow|grey|honeydew|hotpink|indianred|indigo|ivory|khaki|' &&
    'lavender|lavenderblush|lawngreen|lemonchiffon|lightblue|lightcoral|lightcyan|lightgoldenrodyellow|' &&
    'lightgray|lightgreen|lightgrey|lightpink|lightsalmon|lightseagreen|lightskyblue|lightslategray|' &&
    'lightslategrey|lightsteelblue|lightyellow|lime|limegreen|linen|magenta|maroon|mediumaquamarine|' &&
    'mediumblue|mediumorchid|mediumpurple|mediumseagreen|mediumslateblue|mediumspringgreen|mediumturquoise|' &&
    'mediumvioletred|midnightblue|mintcream|mistyrose|moccasin|navajowhite|navy|oldlace|olive|olivedrab|' &&
    'orange|orangered|orchid|palegoldenrod|palegreen|paleturquoise|palevioletred|papayawhip|peachpuff|' &&
    'peru|pink|plum|powderblue|purple|rebeccapurple|red|rosybrown|royalblue|saddlebrown|salmon|sandybrown|' &&
    'seagreen|seashell|sienna|silver|skyblue|slateblue|slategray|slategrey|snow|springgreen|steelblue|' &&
    'tan|teal|thistle|tomato|turquoise|violet|wheat|white|whitesmoke|yellow|yellowgreen'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-colors ).

    " 6) CSS Extensions
    lv_keywords =
    'moz|moz-binding|moz-border-bottom-colors|moz-border-left-colors|moz-border-right-colors|' &&
    'moz-border-top-colors|moz-box-align|moz-box-direction|moz-box-flex|moz-box-ordinal-group|' &&
    'moz-box-orient|moz-box-pack|moz-box-shadow|moz-context-properties|moz-float-edge|' &&
    'moz-force-broken-image-icon|moz-image-region|moz-orient|moz-osx-font-smoothing|' &&
    'moz-outline-radius|moz-outline-radius-bottomleft|moz-outline-radius-bottomright|' &&
    'moz-outline-radius-topleft|moz-outline-radius-topright|moz-stack-sizing|moz-system-metric|' &&
    'moz-transform|moz-transform-origin|moz-transition|moz-transition-delay|moz-user-focus|' &&
    'moz-user-input|moz-user-modify|moz-window-dragging|moz-window-shadow|ms|ms-accelerator|' &&
    'ms-block-progression|ms-content-zoom-chaining|ms-content-zoom-limit|' &&
    'ms-content-zoom-limit-max|ms-content-zoom-limit-min|ms-content-zoom-snap|' &&
    'ms-content-zoom-snap-points|ms-content-zoom-snap-type|ms-content-zooming|ms-filter|' &&
    'ms-flow-from|ms-flow-into|ms-high-contrast-adjust|ms-hyphenate-limit-chars|' &&
    'ms-hyphenate-limit-lines|ms-hyphenate-limit-zone|ms-ime-align|ms-overflow-style|' &&
    'ms-scroll-chaining|ms-scroll-limit|ms-scroll-limit-x-max|ms-scroll-limit-x-min|' &&
    'ms-scroll-limit-y-max|ms-scroll-limit-y-min|ms-scroll-rails|ms-scroll-snap-points-x|' &&
    'ms-scroll-snap-points-y|ms-scroll-snap-x|ms-scroll-snap-y|ms-scroll-translation|' &&
    'ms-scrollbar-3dlight-color|ms-scrollbar-arrow-color|ms-scrollbar-base-color|' &&
    'ms-scrollbar-darkshadow-color|ms-scrollbar-face-color|ms-scrollbar-highlight-color|' &&
    'ms-scrollbar-shadow-color|ms-scrollbar-track-color|ms-transform|ms-text-autospace|' &&
    'ms-touch-select|ms-wrap-flow|ms-wrap-margin|ms-wrap-through|o|o-transform|webkit|' &&
    'webkit-animation-trigger|webkit-app-region|webkit-appearance|webkit-aspect-ratio|' &&
    'webkit-backdrop-filter|webkit-background-composite|webkit-border-after|' &&
    'webkit-border-after-color|webkit-border-after-style|webkit-border-after-width|' &&
    'webkit-border-before|webkit-border-before-color|webkit-border-before-style|' &&
    'webkit-border-before-width|webkit-border-end|webkit-border-end-color|' &&
    'webkit-border-end-style|webkit-border-end-width|webkit-border-fit|' &&
    'webkit-border-horizontal-spacing|webkit-border-radius|webkit-border-start|' &&
    'webkit-border-start-color|webkit-border-start-style|webkit-border-start-width|' &&
    'webkit-border-vertical-spacing|webkit-box-align|webkit-box-direction|webkit-box-flex|' &&
    'webkit-box-flex-group|webkit-box-lines|webkit-box-ordinal-group|webkit-box-orient|' &&
    'webkit-box-pack|webkit-box-reflect|webkit-box-shadow|webkit-column-axis|' &&
    'webkit-column-break-after|webkit-column-break-before|webkit-column-break-inside|' &&
    'webkit-column-progression|webkit-cursor-visibility|webkit-dashboard-region|' &&
    'webkit-font-size-delta|webkit-font-smoothing|webkit-highlight|webkit-hyphenate-character|' &&
    'webkit-hyphenate-limit-after|webkit-hyphenate-limit-before|webkit-hyphenate-limit-lines|' &&
    'webkit-initial-letter|webkit-line-align|webkit-line-box-contain|webkit-line-clamp|' &&
    'webkit-line-grid|webkit-line-snap|webkit-locale|webkit-logical-height|' &&
    'webkit-logical-width|webkit-margin-after|webkit-margin-after-collapse|' &&
    'webkit-margin-before|webkit-margin-before-collapse|webkit-margin-bottom-collapse|' &&
    'webkit-margin-collapse|webkit-margin-end|webkit-margin-start|webkit-margin-top-collapse|' &&
    'webkit-marquee|webkit-marquee-direction|webkit-marquee-increment|' &&
    'webkit-marquee-repetition|webkit-marquee-speed|webkit-marquee-style|webkit-mask-box-image|' &&
    'webkit-mask-box-image-outset|webkit-mask-box-image-repeat|webkit-mask-box-image-slice|' &&
    'webkit-mask-box-image-source|webkit-mask-box-image-width|webkit-mask-repeat-x|' &&
    'webkit-mask-repeat-y|webkit-mask-source-type|webkit-max-logical-height|' &&
    'webkit-max-logical-width|webkit-min-logical-height|webkit-min-logical-width|' &&
    'webkit-nbsp-mode|webkit-padding-after|webkit-padding-before|webkit-padding-end|' &&
    'webkit-padding-start|webkit-perspective-origin-x|webkit-perspective-origin-y|' &&
    'webkit-print-color-adjust|webkit-rtl-ordering|webkit-svg-shadow|' &&
    'webkit-tap-highlight-color|webkit-text-combine|webkit-text-decoration-skip|' &&
    'webkit-text-decorations-in-effect|webkit-text-fill-color|webkit-text-security|' &&
    'webkit-text-stroke|webkit-text-stroke-color|webkit-text-stroke-width|webkit-text-zoom|' &&
    'webkit-transform|webkit-transform-origin|webkit-transform-origin-x|' &&
    'webkit-transform-origin-y|webkit-transform-origin-z|webkit-transition|' &&
    'webkit-transition-delay|webkit-user-drag|webkit-user-modify|overflow-clip-box|' &&
    'overflow-clip-box-block|overflow-clip-box-inline|zoom'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-extensions ).

    " 6) CSS At-Rules
    lv_keywords =
    '@|charset|counter-style|font-face|import|keyframes'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-at_rules ).

    " 7) HTML tage
    lv_keywords =
    'doctyype|a|abbr|acronym|address|applet|area|b|base|basefont|bdo|bgsound|big|blink|blockquote|' &&
    'body|br|button|caption|center|cite|code|col|colgroup|dd|del|dfn|dir|div|dl|dt|em|embed|fieldset|' &&
    'font|form|frame|frameset|h1|h2|h3|h4|h5|h6|head|hr|html|i|iframe|ilayer|img|input|ins|isindex|' &&
    'kbd|keygen|label|layer|legend|li|link|listing|map|menu|meta|multicol|nobr|noembed|noframes|' &&
    'nolayer|noscript|object|ol|optgroup|option|p|param|plaintext|pre|q|s|samp|script|select|server|' &&
    'small|sound|spacer|span|strike|strong|style|sub|sup|tbody|textarea|title|tt|u|ul|var|wbr|xmp|' &&
    'xsl|xml|accesskey|action|align|alink|alt|background|balance|behavior|bgcolor|bgproperties|' &&
    'border|bordercolor|bordercolordark|bordercolorlight|bottommargin|checked|class|classid|clear|' &&
    'code|codebase|codetype|color|cols|colspan|compact|content|controls|coords|data|datafld|' &&
    'dataformatas|datasrc|direction|disabled|dynsrc|enctype|event|face|for|frame|frameborder|' &&
    'framespacing|height|hidden|href|hspace|http-equiv|id|ismap|lang|language|leftmargin|link|loop|' &&
    'lowsrc|marginheight|marginwidth|maxlength|mayscript|method|methods|multiple|name|nohref|' &&
    'noresize|noshade|nowrap|palette|pluginspage|public|readonly|rel|rev|rightmargin|rows|rowspan|' &&
    'rules|scroll|scrollamount|scrolldelay|scrolling|selected|shape|size|span|src|start|style|' &&
    'tabindex|target|text|title|topmargin|truespeed|type|url|urn|usemap|valign|value|vlink|volume|' &&
    'vrml|vspace|width|wrap|apply-templates|attribute|choose|comment|define-template-set|' &&
    'entity-ref|eval|expr|for-each|if|match|no-entities|node-name|order-by|otherwise|select|' &&
    'stylesheet|template|test|value-of|version|when|xmlns|xsl|cellpadding|cellspacing|table|td|' &&
    'tfoot|th|thead|tr'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-html ).

  ENDMETHOD.
  METHOD insert_keywords.

    DATA: lt_keywords TYPE STANDARD TABLE OF string,
          ls_keyword  TYPE ty_keyword.

    FIELD-SYMBOLS: <lv_keyword> TYPE any.

    SPLIT iv_keywords AT '|' INTO TABLE lt_keywords.

    LOOP AT lt_keywords ASSIGNING <lv_keyword>.
      CLEAR ls_keyword.
      ls_keyword-keyword = <lv_keyword>.
      ls_keyword-token = iv_token.
      INSERT ls_keyword INTO TABLE gt_keywords.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_keyword.

    DATA lv_str TYPE string.

    lv_str = to_lower( iv_chunk ).
    READ TABLE gt_keywords WITH TABLE KEY keyword = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_prev_end   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>    TYPE ty_match,
      <ls_match>   TYPE ty_match,
      <ls_keyword> TYPE ty_keyword.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          " Skip keyword that's part of previous (longer) keyword
          IF <ls_match>-offset < lv_prev_end.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

          " Map generic keyword to specific CSS token
          lv_match = to_lower( lv_match ).
          READ TABLE gt_keywords ASSIGNING <ls_keyword> WITH TABLE KEY keyword = lv_match.
          IF sy-subrc = 0.
            <ls_match>-token = <ls_keyword>-token.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '/*'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '*/'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 2.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
          ENDIF.

        WHEN c_token-text.
          <ls_match>-text_tag = lv_match.
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ENDIF.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      lv_prev_end   = <ls_match>-offset + <ls_match>-length.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

  ENDMETHOD.
  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        CLEAR <ls_match>-token.
      ENDIF.
    ENDLOOP.

    DELETE rt_matches WHERE token IS INITIAL.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_CSS implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_factory====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_factory====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD create.

    " Create instance of highighter dynamically dependent on syntax type
    IF iv_filename CP '*.abap'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_abap.
    ELSEIF iv_filename CP '*.xml' OR iv_filename CP '*.html'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_xml.
    ELSEIF iv_filename CP '*.css'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_css.
    ELSEIF iv_filename CP '*.js'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_js.
    ELSEIF iv_filename CP '*.json' OR iv_filename CP '*.jsonc'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_json.
    ELSEIF iv_filename CP '*.txt' OR iv_filename CP '*.ini'  OR iv_filename CP '*.text'.
      CREATE OBJECT ro_instance TYPE Lcl_abapgit_syntax_txt.
    ELSE.
      CLEAR ro_instance.
    ENDIF.

    IF ro_instance IS BOUND.
      ro_instance->set_hidden_chars( iv_hidden_chars ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_HIGHLIGHTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_highlighterccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_highlighterccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_HIGHLIGHTER implementation.
*"* method's implementations
*include methods.
  METHOD add_rule.

    DATA ls_rule LIKE LINE OF mt_rules.

    IF NOT iv_regex IS INITIAL.
      CREATE OBJECT ls_rule-regex
        EXPORTING
          pattern     = iv_regex
          ignore_case = abap_true.
    ENDIF.

    ls_rule-token         = iv_token.
    ls_rule-style         = iv_style.
    ls_rule-relevant_submatch = iv_submatch.
    APPEND ls_rule TO mt_rules.

  ENDMETHOD.
  METHOD apply_style.

    DATA lv_escaped TYPE string.

    lv_escaped = escape( val    = iv_line
                         format = cl_abap_format=>e_html_text ).

    lv_escaped = show_hidden_chars( lv_escaped ).

    IF iv_class IS NOT INITIAL.
      rv_line = |<span class="{ iv_class }">{ lv_escaped }</span>|.
    ELSE.
      rv_line = lv_escaped.
    ENDIF.

  ENDMETHOD.
  METHOD extend_matches.

    DATA: lv_line_len TYPE i,
          lv_last_pos TYPE i VALUE 0,
          lv_length   TYPE i,
          ls_match    TYPE ty_match.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    lv_line_len = strlen( iv_line ).

    SORT ct_matches BY offset.

    " Add entries refering to parts of text that should not be formatted
    LOOP AT ct_matches ASSIGNING <ls_match>.
      IF <ls_match>-offset > lv_last_pos.
        lv_length = <ls_match>-offset - lv_last_pos.
        ls_match-token  = c_token_none.
        ls_match-offset = lv_last_pos.
        ls_match-length = lv_length.
        INSERT ls_match INTO ct_matches INDEX sy-tabix.
      ENDIF.
      lv_last_pos = <ls_match>-offset + <ls_match>-length.
    ENDLOOP.

    " Add remainder of the string
    IF lv_line_len > lv_last_pos.
      lv_length = lv_line_len - lv_last_pos.
      ls_match-token  = c_token_none.
      ls_match-offset = lv_last_pos.
      ls_match-length = lv_length.
      APPEND ls_match TO ct_matches.
    ENDIF.

  ENDMETHOD.
  METHOD format_line.

    DATA:
      lv_chunk TYPE string,
      ls_rule  LIKE LINE OF mt_rules.

    FIELD-SYMBOLS <ls_match> TYPE ty_match.

    LOOP AT it_matches ASSIGNING <ls_match>.
      lv_chunk = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CLEAR ls_rule. " Failed read equals no style
      READ TABLE mt_rules INTO ls_rule WITH KEY token = <ls_match>-token.

      lv_chunk = apply_style( iv_line  = lv_chunk
                              iv_class = ls_rule-style ).

      rv_line = rv_line && lv_chunk.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_whitespace.

    DATA: lv_whitespace TYPE string.

    "/^\s+$/
    lv_whitespace = ` ` && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.

    rv_result = boolc( iv_string CO lv_whitespace ).

  ENDMETHOD.
  METHOD order_matches.
  ENDMETHOD.
  METHOD parse_line.

    DATA:
      lo_regex   TYPE REF TO cl_abap_regex,
      lo_matcher TYPE REF TO cl_abap_matcher,
      lt_result  TYPE match_result_tab,
      ls_match   TYPE ty_match.

    FIELD-SYMBOLS:
      <ls_regex>    LIKE LINE OF mt_rules,
      <ls_result>   TYPE match_result,
      <ls_submatch> LIKE LINE OF <ls_result>-submatches.


    " Process syntax-dependent regex table and find all matches
    LOOP AT mt_rules ASSIGNING <ls_regex> WHERE regex IS BOUND.
      lo_regex   = <ls_regex>-regex.
      lo_matcher = lo_regex->create_matcher( text = iv_line ).
      lt_result  = lo_matcher->find_all( ).

      " Save matches into custom table with predefined tokens
      LOOP AT lt_result ASSIGNING <ls_result>.
        CLEAR: ls_match.
        IF <ls_regex>-relevant_submatch = 0.
          ls_match-token  = <ls_regex>-token.
          ls_match-offset = <ls_result>-offset.
          ls_match-length = <ls_result>-length.
          APPEND ls_match TO rt_matches.
        ELSE.
          READ TABLE <ls_result>-submatches ASSIGNING <ls_submatch> INDEX <ls_regex>-relevant_submatch.
          "submatch might be empty if only discarted parts matched
          IF sy-subrc = 0 AND <ls_submatch>-offset >= 0 AND <ls_submatch>-length > 0.
            ls_match-token  = <ls_regex>-token.
            ls_match-offset = <ls_submatch>-offset.
            ls_match-length = <ls_submatch>-length.
            APPEND ls_match TO rt_matches.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD process_line.

    DATA: lt_matches TYPE ty_match_tt.

    IF iv_line IS INITIAL OR is_whitespace( iv_line ) = abap_true.
      rv_line = show_hidden_chars( iv_line ).
      RETURN.
    ENDIF.

    lt_matches = parse_line( iv_line ).

    order_matches( EXPORTING iv_line    = iv_line
                   CHANGING  ct_matches = lt_matches ).

    extend_matches( EXPORTING iv_line    = iv_line
                    CHANGING  ct_matches = lt_matches ).

    rv_line = format_line( iv_line    = iv_line
                           it_matches = lt_matches ).

  ENDMETHOD.
  METHOD set_hidden_chars.
    mv_hidden_chars = iv_hidden_chars.
  ENDMETHOD.
  METHOD show_hidden_chars.

    DATA lv_bom TYPE x LENGTH 3.

    rv_line = iv_line.

    IF mv_hidden_chars = abap_true.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_line WITH '&nbsp;&rarr;&nbsp;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1)       IN rv_line WITH '&para;'.
      REPLACE ALL OCCURRENCES OF ` `                                    IN rv_line WITH '&middot;'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN rv_line
        WITH '<span class="red">&odash;</span>'.

      IF strlen( rv_line ) BETWEEN 1 AND 2.
        TRY.
            lv_bom = Lcl_abapgit_convert=>string_to_xstring( rv_line ).
          CATCH Lcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
        IF lv_bom(2) = cl_abap_char_utilities=>byte_order_mark_big.
          rv_line = '<span class="red">&squf;</span>'. " UTF-16 big-endian (FE FF)
        ENDIF.
        IF lv_bom(2) = cl_abap_char_utilities=>byte_order_mark_little.
          rv_line = '<span class="red">&compfn;</span>'. " UTF-16 little-endian (FF FE)
        ENDIF.
        IF lv_bom(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
          rv_line = '<span class="red">&curren;</span>'. " UTF-8 (EF BB BF)
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_HIGHLIGHTER implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_JS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_js=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_js=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_JS implementation.
*"* method's implementations
*include methods.
  METHOD class_constructor.

    init_keywords( ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

    " Initialize instances of regular expression
    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    " Styles for keywords
    add_rule( iv_regex = ''
              iv_token = c_token-variables
              iv_style = c_css-variables ).

  ENDMETHOD.
  METHOD init_keywords.

    DATA: lv_keywords TYPE string.

    CLEAR gt_keywords.

    " 1) General keywords
    lv_keywords =
    'alert|all|body|break|bytetostring|case|continue|default|delete|do|document|else|event|export|for|function|if|' &&
    'import|in|innerhtml|isnan|item|mimetypes|navigator|new|onabort|onblur|onchange|onclick|ondblclick|ondragdrop|' &&
    'onerror|onfocus|onkeydown|onkeypress|onkeyup|onload|onmousedown|onmousemove|onmouseout|onmouseover|onmouseup|' &&
    'onmove|onreset|onselect|onsubmit|onunload|onresize|options|parsefloat|parseint|prototype|return|screen|switch|' &&
    'unit|var|void|while|window|with|anchor|applet|area|button|checkbox|fileupload|form|frame|hidden|link|mimetype|' &&
    'password|plugin|radio|reset|select|submit|text|textarea|abs|acos|alert|anchor|asin|atan|atan2|back|big|blink|' &&
    'blur|bold|captureevents|ceil|charat|charcodeat|clearinterval|cleartimeout|click|close|concat|confirm|cos|' &&
    'disableexternalcapture|enableexternalcapture|eval|exp|find|fixed|floor|focus|fontcolor|fontsize|forward|' &&
    'fromcharcode|getdate|getday|getelementbyid|gethours|getminutes|getmonth|getoptionvalue|getoptionvaluecount|' &&
    'getseconds|getselection|gettime|gettimezoneoffset|getyear|go|handleevent|home|indexof|italics|javaenabled|join|' &&
    'lastindexof|link|load|log|match|max|min|moveabove|movebelow|moveby|moveto|movetoabsolute|open|parse|plugins|' &&
    'pop|pow|preference|print|prompt|push|random|refresh|releaseevents|reload|replace|reset|resizeby|resizeto|' &&
    'reverse|round|routeevent|scroll|scrollby|scrollto|search|select|setdate|sethours|setinterval|setminutes|' &&
    'setmonth|setseconds|settime|settimeout|setyear|shift|sin|slice|small|sort|splice|split|sqrt|stop|strike|sub|' &&
    'submit|substr|substring|sup|taintenabled|tan|togmtstring|tolocalestring|tolowercase|tostring|touppercase|' &&
    'unshift|unwatch|utc|valueof|watch|write|writeln|e|ln10|ln2|log10e|log2e|max_value|min_value|negative_infinity|' &&
    'nan|pi|positive_infinity|url|above|action|alinkcolor|anchors|appcodename|appname|appversion|applets|arguments|' &&
    'arity|availheight|availwidth|background|backgroundcolor|below|bgcolor|border|bottom|caller|cancelbubble|' &&
    'checked|clientheight|clientwidth|clientx|clienty|clip|closed|color|colordepth|complete|constructor|cookie|' &&
    'count|current|defaultchecked|defaultselected|defaultstatus|defaultvalue|description|display|document|domain|' &&
    'elements|embeds|enabledplugin|encoding|false|fgcolor|filename|form|formname|forms|frames|hash|height|history|' &&
    'host|hostname|href|hspace|images|innerheight|innerwidth|language|lastmodified|layers|left|length|linkcolor|' &&
    'links|location|locationbar|lowsrc|menubar|method|mimetypes|name|next|null|offsetheight|offsetleft|offsetparent|' &&
    'offsetwidth|opener|outerheight|outerwidth|pagex|pagexoffset|pagey|pageyoffset|parent|parentlayer|pathname|' &&
    'personalbar|pixeldepth|platform|plugins|port|poswidth|previous|protocol|prototype|referrer|right|scrolltop|' &&
    'scrollbars|search|selected|selectedindex|self|siblingabove|siblingbelow|src|srcelement|status|statusbar|style|' &&
    'suffixes|tags|target|text|this|title|toolbar|top|true|type|useragent|value|visibility|vlinkcolor|vspace|width|' &&
    'window|zindex'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-keyword ).

    " 2) Variable types
    lv_keywords =
    'array|boolean|date|function|image|layer|math|number|object|option|regexp|string'.
    insert_keywords( iv_keywords = lv_keywords
                     iv_token = c_token-variables ).

  ENDMETHOD.
  METHOD insert_keywords.

    DATA: lt_keywords TYPE STANDARD TABLE OF string,
          ls_keyword  TYPE ty_keyword.

    FIELD-SYMBOLS: <lv_keyword> TYPE any.

    SPLIT iv_keywords AT '|' INTO TABLE lt_keywords.

    LOOP AT lt_keywords ASSIGNING <lv_keyword>.
      CLEAR ls_keyword.
      ls_keyword-keyword = <lv_keyword>.
      ls_keyword-token = iv_token.
      INSERT ls_keyword INTO TABLE gt_keywords.
    ENDLOOP.

  ENDMETHOD.
  METHOD is_keyword.

    DATA lv_str TYPE string.

    lv_str = to_lower( iv_chunk ).
    READ TABLE gt_keywords WITH TABLE KEY keyword = lv_str TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_prev_end   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>    TYPE ty_match,
      <ls_match>   TYPE ty_match,
      <ls_keyword> TYPE ty_keyword.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-keyword.
          " Skip keyword that's part of previous (longer) keyword
          IF <ls_match>-offset < lv_prev_end.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

          " Map generic keyword to specific token
          lv_match = to_lower( lv_match ).
          READ TABLE gt_keywords ASSIGNING <ls_keyword> WITH TABLE KEY keyword = lv_match.
          IF sy-subrc = 0.
            <ls_match>-token = <ls_keyword>-token.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '/*'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '//'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
          ELSEIF lv_match = '*/'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 2.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
          ENDIF.

        WHEN c_token-text.
          <ls_match>-text_tag = lv_match.
          IF lv_prev_token = c_token-text.
            IF <ls_match>-text_tag = <ls_prev>-text_tag.
              <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
              CLEAR lv_prev_token.
            ENDIF.
            CLEAR <ls_match>-token.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      lv_prev_end   = <ls_match>-offset + <ls_match>-length.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

  ENDMETHOD.
  METHOD parse_line. "REDEFINITION

    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_match> LIKE LINE OF rt_matches.

    rt_matches = super->parse_line( iv_line ).

    " Remove non-keywords
    LOOP AT rt_matches ASSIGNING <ls_match> WHERE token = c_token-keyword.
      lv_index = sy-tabix.
      IF abap_false = is_keyword( substring( val = iv_line
                                             off = <ls_match>-offset
                                             len = <ls_match>-length ) ).
        CLEAR <ls_match>-token.
      ENDIF.
    ENDLOOP.

    DELETE rt_matches WHERE token IS INITIAL.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_JS implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_JSON <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_json=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_json=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_syntax_json=======ccau.



class LCL_ABAPGIT_SYNTAX_JSON implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    " Initialize instances of regular expression

    add_rule( iv_regex = c_regex-keyword
              iv_token = c_token-keyword
              iv_style = c_css-keyword ).

    " Style for keys
    add_rule( iv_regex = c_regex-text
              iv_token = c_token-text
              iv_style = c_css-text ).

    " Style for values
    add_rule( iv_regex = ''
              iv_token = c_token-values
              iv_style = c_css-values ).

    " JSONC comments
    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_count      TYPE i,
      lv_line_len   TYPE i,
      lv_prev_token TYPE c.

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    " Longest matches
    SORT ct_matches BY offset length DESCENDING.

    lv_line_len = strlen( iv_line ).

    LOOP AT ct_matches ASSIGNING <ls_match>.
      " Delete matches after open text match
      IF lv_prev_token = c_token-text AND <ls_match>-token <> c_token-text.
        CLEAR <ls_match>-token.
        CONTINUE.
      ENDIF.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      IF <ls_match>-token = c_token-text.
        <ls_match>-text_tag = lv_match.
        IF lv_prev_token = c_token-text.
          IF <ls_match>-text_tag = <ls_prev>-text_tag.
            <ls_prev>-length = <ls_match>-offset + <ls_match>-length - <ls_prev>-offset.
            CLEAR lv_prev_token.
          ENDIF.
          CLEAR <ls_match>-token.
          CONTINUE.
        ENDIF.
      ENDIF.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    DELETE ct_matches WHERE token IS INITIAL.

    " Switch style of second text match to values
    LOOP AT ct_matches ASSIGNING <ls_match> WHERE token = c_token-text.
      lv_count = lv_count + 1.
      IF lv_count >= 2.
        <ls_match>-token = c_token-values.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_JSON implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_TXT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_txt========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_txt========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SYNTAX_TXT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    " No rules for plain text files

  ENDMETHOD.
  METHOD process_line.

    rv_line = apply_style(
      iv_line  = iv_line
      iv_class = '' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_TXT implementation

*>>>>>>> ZCL_ABAPGIT_SYNTAX_XML <<<<<<<*

*"* macro definitions
*include zcl_abapgit_syntax_xml========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_syntax_xml========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_syntax_xml========ccau.



*CLASS SHRITEFUH64VYIPO5IWUYCIJCOASFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_syntax_xml DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCOASFY.

*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYCIJCOASFY definition
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS SHRITEFUH64VYIPO5IWUYCIJCOASFY IMPLEMENTATION
*----------------------------------------------------------------------*

class LCL_ABAPGIT_SYNTAX_XML implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    " Reset indicator for multi-line comments
    CLEAR gv_comment.

    " Initialize instances of regular expressions
    add_rule( iv_regex    = c_regex-xml_tag
              iv_token    = c_token-xml_tag
              iv_style    = c_css-xml_tag
              iv_submatch = 1 ).

    add_rule( iv_regex = c_regex-attr
              iv_token = c_token-attr
              iv_style = c_css-attr ).

    add_rule( iv_regex = c_regex-attr_val
              iv_token = c_token-attr_val
              iv_style = c_css-attr_val ).

    add_rule( iv_regex = c_regex-comment
              iv_token = c_token-comment
              iv_style = c_css-comment ).

  ENDMETHOD.
  METHOD order_matches.

    DATA:
      lv_match      TYPE string,
      lv_line_len   TYPE i,
      lv_cmmt_end   TYPE i,
      lv_index      TYPE sy-tabix,
      lv_prev_token TYPE c,
      lv_state      TYPE c VALUE 'O'. " O - for open tag; C - for closed tag;

    FIELD-SYMBOLS:
      <ls_prev>  TYPE ty_match,
      <ls_match> TYPE ty_match.

    SORT ct_matches BY offset.

    lv_line_len = strlen( iv_line ).

    " Check if this is part of multi-line comment and mark it accordingly
    IF gv_comment = abap_true.
      READ TABLE ct_matches WITH KEY token = c_token-comment TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ct_matches.
        APPEND INITIAL LINE TO ct_matches ASSIGNING <ls_match>.
        <ls_match>-token = c_token-comment.
        <ls_match>-offset = 0.
        <ls_match>-length = lv_line_len.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT ct_matches ASSIGNING <ls_match>.
      lv_index = sy-tabix.

      lv_match = substring( val = iv_line
                            off = <ls_match>-offset
                            len = <ls_match>-length ).

      CASE <ls_match>-token.
        WHEN c_token-xml_tag.
          <ls_match>-text_tag = lv_match.

          " No other matches between two tags
          IF <ls_match>-text_tag = '>' AND lv_prev_token = c_token-xml_tag.
            lv_state = 'C'.
            <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset + <ls_match>-length.
            DELETE ct_matches INDEX lv_index.
            CONTINUE.

            " Adjust length and offset of closing tag
          ELSEIF <ls_match>-text_tag = '>' AND lv_prev_token <> c_token-xml_tag.
            lv_state = 'C'.
            IF <ls_prev> IS ASSIGNED.
              <ls_match>-length = <ls_match>-offset - <ls_prev>-offset - <ls_prev>-length + <ls_match>-length.
              <ls_match>-offset = <ls_prev>-offset + <ls_prev>-length.
            ENDIF.
          ELSE.
            lv_state = 'O'.
          ENDIF.

        WHEN c_token-comment.
          IF lv_match = '<!--'.
            DELETE ct_matches WHERE offset > <ls_match>-offset.
            DELETE ct_matches WHERE offset = <ls_match>-offset AND token = c_token-xml_tag.
            <ls_match>-length = lv_line_len - <ls_match>-offset.
            gv_comment = abap_true.
          ELSEIF lv_match = '-->'.
            DELETE ct_matches WHERE offset < <ls_match>-offset.
            <ls_match>-length = <ls_match>-offset + 3.
            <ls_match>-offset = 0.
            gv_comment = abap_false.
          ELSE.
            lv_cmmt_end = <ls_match>-offset + <ls_match>-length.
            DELETE ct_matches WHERE offset > <ls_match>-offset AND offset <= lv_cmmt_end.
            DELETE ct_matches WHERE offset = <ls_match>-offset AND token = c_token-xml_tag.
          ENDIF.

        WHEN OTHERS.
          IF lv_prev_token = c_token-xml_tag.
            <ls_prev>-length = <ls_match>-offset - <ls_prev>-offset. " Extend length of the opening tag
          ENDIF.

          IF lv_state = 'C'.  " Delete all matches between tags
            DELETE ct_matches INDEX lv_index.
            CONTINUE.
          ENDIF.

      ENDCASE.

      lv_prev_token = <ls_match>-token.
      ASSIGN <ls_match> TO <ls_prev>.
    ENDLOOP.

    "if the last XML tag is not closed, extend it to the end of the tag
    IF lv_prev_token = c_token-xml_tag
        AND <ls_prev> IS ASSIGNED
        AND <ls_prev>-length  = 1
        AND <ls_prev>-text_tag = '<'.

      FIND REGEX '<\s*[^\s]*' IN iv_line+<ls_prev>-offset MATCH LENGTH <ls_prev>-length.
      IF sy-subrc <> 0.
        <ls_prev>-length = 1.
      ENDIF.

    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SYNTAX_XML implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SXCI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sxci=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sxci=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SXCI implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE uname FROM sxc_attr INTO rv_user WHERE imp_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_implementation_name TYPE rsexscrn-imp_name.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXO_IMPL_DELETE'
      EXPORTING
        imp_name           = lv_implementation_name
        no_dialog          = abap_true
      EXCEPTIONS
        imp_not_existing   = 1
        action_canceled    = 2
        access_failure     = 3
        data_inconsistency = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_badi_definition             TYPE badi_data,
          lo_filter_object               TYPE REF TO cl_badi_flt_struct,
          lo_filter_values_object        TYPE REF TO cl_badi_flt_values_alv,
          lv_korrnum                     TYPE trkorr,
          lv_filter_type_enhanceability  TYPE rsexscrn-flt_ext,
          lv_package                     TYPE devclass,
          ls_classic_badi_implementation TYPE ty_classic_badi_implementation.

    io_xml->read(
      EXPORTING
        iv_name = 'SXCI'
      CHANGING
        cg_data = ls_classic_badi_implementation ).

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = ls_classic_badi_implementation-implementation_data-exit_name
      IMPORTING
        badi         = ls_badi_definition
        filter_obj   = lo_filter_object
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_package = iv_package.

    CREATE OBJECT lo_filter_values_object
      EXPORTING
        filter_object = lo_filter_object
        filter_values = ls_classic_badi_implementation-filters.

    CALL FUNCTION 'SXO_IMPL_SAVE'
      EXPORTING
        impl             = ls_classic_badi_implementation-implementation_data
        flt_ext          = lv_filter_type_enhanceability
        filter_val_obj   = lo_filter_values_object
        genflag          = abap_true
        no_dialog        = abap_true
      TABLES
        fcodes_to_insert = ls_classic_badi_implementation-function_codes
        cocos_to_insert  = ls_classic_badi_implementation-control_composites
        intas_to_insert  = ls_classic_badi_implementation-customer_includes
        sscrs_to_insert  = ls_classic_badi_implementation-screens
      CHANGING
        korrnum          = lv_korrnum
        devclass         = lv_package
      EXCEPTIONS
        save_failure     = 1
        action_canceled  = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_ACTIVE'
      EXPORTING
        imp_name                  = ls_classic_badi_implementation-implementation_data-imp_name
        no_dialog                 = abap_true
      EXCEPTIONS
        badi_not_existing         = 1
        imp_not_existing          = 2
        already_active            = 3
        data_inconsistency        = 4
        activation_not_admissable = 5
        action_canceled           = 6
        access_failure            = 7
        OTHERS                    = 8.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_implementation_name TYPE rsexscrn-imp_name.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_IMP_EXISTS'
      EXPORTING
        imp_name           = lv_implementation_name
      EXCEPTIONS
        not_existing       = 1
        data_inconsistency = 2
        OTHERS             = 3.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).

    "Note: SAP does not show inactive classic BAdIs as "Inactive objects" in SE80
    "Therefore, rv_active will always be true. The implementation state (runtime
    "behaviour of the BAdI) will be serialized as part of the XML
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_implementation_name         TYPE rsexscrn-imp_name,
          lv_exit_name                   TYPE rsexscrn-exit_name,
          lo_filter_object               TYPE REF TO cl_badi_flt_struct,
          ls_badi_definition             TYPE badi_data,
          lo_filter_values_object        TYPE REF TO cl_badi_flt_values_alv,
          lt_methods                     TYPE seex_mtd_table,
          ls_classic_badi_implementation TYPE ty_classic_badi_implementation.

    lv_implementation_name = ms_item-obj_name.

    CALL FUNCTION 'SXV_EXIT_FOR_IMP'
      EXPORTING
        imp_name           = lv_implementation_name
      IMPORTING
        exit_name          = lv_exit_name
      TABLES
        filters            = ls_classic_badi_implementation-filters
      EXCEPTIONS
        data_inconsistency = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SXO_BADI_READ'
      EXPORTING
        exit_name    = lv_exit_name
      IMPORTING
        badi         = ls_badi_definition
        filter_obj   = lo_filter_object
      TABLES
        fcodes       = ls_classic_badi_implementation-function_codes
        cocos        = ls_classic_badi_implementation-control_composites
        intas        = ls_classic_badi_implementation-customer_includes
        scrns        = ls_classic_badi_implementation-screens
        methods      = lt_methods
      EXCEPTIONS
        read_failure = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SXO_IMPL_FOR_BADI_READ'
      EXPORTING
        imp_name                    = lv_implementation_name
        exit_name                   = lv_exit_name
        inter_name                  = ls_badi_definition-inter_name
        filter_obj                  = lo_filter_object
        no_create_filter_values_obj = abap_true
      IMPORTING
        impl                        = ls_classic_badi_implementation-implementation_data
        filter_values_obj           = lo_filter_values_object
      TABLES
        fcodes                      = ls_classic_badi_implementation-function_codes
        cocos                       = ls_classic_badi_implementation-control_composites
        intas                       = ls_classic_badi_implementation-customer_includes
        scrns                       = ls_classic_badi_implementation-screens
      CHANGING
        methods                     = lt_methods
      EXCEPTIONS
        read_failure                = 1
        OTHERS                      = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR: ls_classic_badi_implementation-implementation_data-aname,
           ls_classic_badi_implementation-implementation_data-adate,
           ls_classic_badi_implementation-implementation_data-atime,
           ls_classic_badi_implementation-implementation_data-uname,
           ls_classic_badi_implementation-implementation_data-udate,
           ls_classic_badi_implementation-implementation_data-utime.

    io_xml->add( iv_name = 'SXCI'
                 ig_data = ls_classic_badi_implementation ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SXCI implementation

*>>>>>>> ZCL_ABAPGIT_TRANSPORT_OBJECTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport_objects=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport_objects=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_transport_objects=ccau.




class LCL_ABAPGIT_TRANSPORT_OBJECTS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mt_transport_objects = it_transport_objects.
  ENDMETHOD.
  METHOD to_stage.
    DATA: ls_transport_object LIKE LINE OF mt_transport_objects,
          ls_local_file       TYPE Lif_abapgit_definitions=>ty_file_item,
          ls_object_status    TYPE Lif_abapgit_definitions=>ty_result.

    LOOP AT mt_transport_objects INTO ls_transport_object.
      LOOP AT it_object_statuses INTO ls_object_status
          WHERE obj_name = ls_transport_object-obj_name
          AND obj_type = ls_transport_object-object
          AND NOT lstate IS INITIAL.

        CASE ls_object_status-lstate.
          WHEN Lif_abapgit_definitions=>c_state-added OR Lif_abapgit_definitions=>c_state-modified.
            IF ls_transport_object-delflag = abap_true.
              Lcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
                } should be added/modified, but has deletion flag in transport| ).
            ENDIF.

            READ TABLE is_stage_objects-local
                  INTO ls_local_file
              WITH KEY item-obj_name = ls_transport_object-obj_name
                       item-obj_type = ls_transport_object-object
                       file-filename = ls_object_status-filename.
            IF sy-subrc <> 0.
              Lcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
                } not found in the local repository files| ).
            ELSE.
              io_stage->add(
                iv_path     = ls_local_file-file-path
                iv_filename = ls_local_file-file-filename
                iv_data     = ls_local_file-file-data ).
            ENDIF.
          WHEN Lif_abapgit_definitions=>c_state-deleted.
* SUSC, see https://github.com/abapGit/abapGit/issues/2772
            IF ls_transport_object-delflag = abap_false
                AND ls_transport_object-object <> 'SUSC'
                AND ls_transport_object-object <> 'IWOM'
                AND ls_transport_object-object <> 'IWMO'
                AND ls_transport_object-object <> 'IWSG'
                AND ls_transport_object-object <> 'IWSV'.
              Lcx_abapgit_exception=>raise( |Object { ls_transport_object-obj_name
                } should be removed, but has NO deletion flag in transport| ).
            ENDIF.
            io_stage->rm(
              iv_path     = ls_object_status-path
              iv_filename = ls_object_status-filename ).
          WHEN OTHERS.
            ASSERT 0 = 1. "Unexpected state
        ENDCASE.
      ENDLOOP.
      IF sy-subrc <> 0.
        " Since not all objects in a transport might be in the local repo
        " i.e generated SADL objects, we don't add these objects to
        " the stage.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TRANSPORT_OBJECTS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TABL_COMPAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tabl_comparccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tabl_comparccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TABL_COMPAR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    mi_local = ii_local.

  ENDMETHOD.
  METHOD get_where_used_recursive.

    DATA: lt_findstrings TYPE string_table,
          lt_founds      TYPE STANDARD TABLE OF rsfindlst,
          lt_scope       TYPE ty_seu_obj,
          lv_findstring  LIKE LINE OF lt_findstrings.

    FIELD-SYMBOLS: <ls_found> TYPE rsfindlst.

    IF iv_object_name IS INITIAL.
      RETURN.
    ENDIF.

    lt_scope = it_scope.

    lv_findstring = iv_object_name.
    INSERT lv_findstring INTO TABLE lt_findstrings.

    DO iv_depth TIMES.

      CLEAR: lt_founds.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = iv_object_type
          no_dialog                = 'X'
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.

      IF sy-subrc = 1 OR sy-subrc = 2 OR lines( lt_founds ) = 0.
        EXIT.
      ELSEIF sy-subrc > 2.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      INSERT LINES OF lt_founds INTO TABLE rt_founds_all.

      CLEAR: lt_findstrings.

      LOOP AT lt_founds ASSIGNING <ls_found>.

        lv_findstring = <ls_found>-object.
        INSERT lv_findstring INTO TABLE lt_findstrings.

      ENDLOOP.

    ENDDO.

  ENDMETHOD.
  METHOD is_structure_used_in_db_table.

    DATA: lt_scope  TYPE ty_seu_obj,
          lt_founds TYPE ty_founds.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.

    lt_founds = get_where_used_recursive( iv_object_name = iv_object_name
                                          iv_object_type = 'STRU'
                                          it_scope       = lt_scope
                                          iv_depth       = 5 ).

    DELETE lt_founds WHERE object_cls <> 'DT'.

    rv_is_structure_used_in_db_tab = boolc( lines( lt_founds ) > 0 ).

  ENDMETHOD.
  METHOD validate.

    DATA: lt_previous_table_fields TYPE TABLE OF dd03p,
          ls_previous_table_field  LIKE LINE OF lt_previous_table_fields,
          lt_current_table_fields  TYPE TABLE OF dd03p,
          ls_current_table_field   LIKE LINE OF lt_current_table_fields,
          ls_dd02v                 TYPE dd02v,
          ls_item                  TYPE Lif_abapgit_definitions=>ty_item,
          lv_inconsistent          TYPE abap_bool.

    FIELD-SYMBOLS <lv_is_gtt> TYPE abap_bool.

    ii_remote_version->read(
      EXPORTING
        iv_name = 'DD02V'
      CHANGING
        cg_data = ls_dd02v ).

    " We only want to compare transparent tables, or structures used in transparent tables
    IF ls_dd02v-tabclass <> 'TRANSP' AND is_structure_used_in_db_table( ls_dd02v-tabname ) = abap_false.
      RETURN.
    ENDIF.

    " No comparison for global temporary tables
    ASSIGN COMPONENT 'IS_GTT' OF STRUCTURE ls_dd02v TO <lv_is_gtt>.
    IF sy-subrc = 0 AND <lv_is_gtt> = abap_true.
      RETURN.
    ENDIF.

    ii_remote_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_previous_table_fields ).

    ii_local_version->read(
      EXPORTING
        iv_name       = 'DD03P_TABLE'
      CHANGING
        cg_data       = lt_current_table_fields ).

    ls_item-obj_name = ls_dd02v-tabname.
    ls_item-obj_type = 'TABL'.

    LOOP AT lt_previous_table_fields INTO ls_previous_table_field.
      READ TABLE lt_current_table_fields WITH KEY fieldname = ls_previous_table_field-fieldname
        INTO ls_current_table_field.
      IF sy-subrc = 0.
        IF ls_current_table_field-rollname <> ls_previous_table_field-rollname.
          IF ls_current_table_field-rollname IS NOT INITIAL AND ls_previous_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data element changed from { ls_previous_table_field-rollname } | &
                        |to { ls_current_table_field-rollname }|
              is_item = ls_item ).
          ELSEIF ls_current_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data type changed from internal type | &
                        |{ ls_previous_table_field-inttype }(length { ls_previous_table_field-intlen }) | &
                        |to data element { ls_current_table_field-rollname }|
              is_item = ls_item ).
          ELSEIF ls_previous_table_field-rollname IS NOT INITIAL.
            ii_log->add_info(
              iv_msg  = |Field { ls_previous_table_field-fieldname }: | &
                        |Data type changed from date element { ls_previous_table_field-rollname } | &
                        |to internal type | &
                        |{ ls_current_table_field-inttype }(length { ls_current_table_field-intlen })|
              is_item = ls_item ).
          ENDIF.
          "TODO: perform several other checks, e.g. field length truncated, ...
          lv_inconsistent = abap_true.
        ENDIF.
      ELSE.
        ii_log->add_info( iv_msg = |Field { ls_previous_table_field-fieldname } removed|
                          is_item = ls_item ).
        lv_inconsistent = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_inconsistent = abap_true.
      rv_message = |Database Table { ls_dd02v-tabname }: Fields were changed. This may lead to inconsistencies!|.
    ENDIF.

    IF NOT rv_message IS INITIAL.
      rv_message = |Database Table { ls_dd02v-tabname }: { rv_message }|.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_comparator~compare.

    rs_result-text = validate(
      ii_remote_version = ii_remote
      ii_local_version  = mi_local
      ii_log            = ii_log ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_TABL_COMPAR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TTYP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ttyp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ttyp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TTYP implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd40l INTO rv_user
      WHERE typename = ms_item-obj_name
      AND as4local = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'A' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v,
          lv_msg   TYPE string.

    io_xml->read( EXPORTING iv_name = 'DD40V'
                  CHANGING cg_data = ls_dd40v ).

    io_xml->read( EXPORTING iv_name = 'DD42V'
                  CHANGING cg_data = lt_dd42v ).
    io_xml->read( EXPORTING iv_name = 'DD43V'
                  CHANGING cg_data = lt_dd43v ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = lv_name
        dd40v_wa          = ls_dd40v
      TABLES
        dd42v_tab         = lt_dd42v
        dd43v_tab         = lt_dd43v
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.

    IF sy-subrc <> 0.
      lv_msg = |Error in DDIF_TTYP_PUT on object { lv_name }|.

      CASE sy-subrc.
        WHEN 1.
          lv_msg = lv_msg && | (TTYP_NOT_FOUND)|.
        WHEN 2.
          lv_msg = lv_msg && | (NAME_INCONSISTENT)|.
        WHEN 3.
          lv_msg = lv_msg && | (TTYP_INCONSISTENT)|.
        WHEN 4.
          lv_msg = lv_msg && | (PUT_FAILURE)|.
        WHEN 5.
          lv_msg = lv_msg && | (PUT_REFUSED)|.
        WHEN OTHERS.
      ENDCASE.

      Lcx_abapgit_exception=>raise( lv_msg ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_ttyp ).

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_typename TYPE dd40l-typename.

    SELECT SINGLE typename FROM dd40l INTO lv_typename
      WHERE typename = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDICT'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lv_state TYPE ddgotstate,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd40v_wa      = ls_dd40v
      TABLES
        dd42v_tab     = lt_dd42v
        dd43v_tab     = lt_dd43v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd40v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd40v-as4user,
           ls_dd40v-as4date,
           ls_dd40v-as4time.

    IF NOT ls_dd40v-rowkind IS INITIAL.
      CLEAR ls_dd40v-typelen.
    ENDIF.

    io_xml->add( iv_name = 'DD40V'
                 ig_data = ls_dd40v ).
    io_xml->add( iv_name = 'DD42V'
                 ig_data = lt_dd42v ).
    io_xml->add( iv_name = 'DD43V'
                 ig_data = lt_dd43v ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_ttyp ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_TTYP implementation

*>>>>>>> ZCL_ABAPGIT_PERSISTENCE_USER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persistence_user==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persistence_user==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_persistence_user==ccau.


class LCL_ABAPGIT_PERSISTENCE_USER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_user = iv_user.
    read( ).
  ENDMETHOD.
  METHOD from_xml.

    DATA: lv_xml TYPE string.

    lv_xml = iv_xml.

* fix downward compatibility
    REPLACE ALL OCCURRENCES OF '<_--28C_TYPE_USER_--29>' IN lv_xml WITH '<USER>'.
    REPLACE ALL OCCURRENCES OF '</_--28C_TYPE_USER_--29>' IN lv_xml WITH '</USER>'.

    CALL TRANSFORMATION id
      OPTIONS value_handling = 'accept_data_loss'
      SOURCE XML lv_xml
      RESULT user = rs_user.
  ENDMETHOD.
  METHOD get_instance.

    IF iv_user = sy-uname ##USER_OK.
      IF gi_current_user IS NOT BOUND.
        CREATE OBJECT gi_current_user TYPE Lcl_abapgit_persistence_user.
      ENDIF.
      ri_user = gi_current_user.
    ELSE.
      CREATE OBJECT ri_user TYPE Lcl_abapgit_persistence_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.
  METHOD read.

    DATA: lv_xml TYPE string.

    TRY.
        lv_xml = Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = Lcl_abapgit_persistence_db=>c_type_user
          iv_value = mv_user ).
      CATCH Lcx_abapgit_not_found.
        RETURN.
    ENDTRY.

    ms_user = from_xml( lv_xml ).

  ENDMETHOD.
  METHOD read_repo_config.
    DATA lv_url TYPE string.
    lv_url = to_lower( iv_url ).
    READ TABLE ms_user-repo_config INTO rs_repo_config WITH KEY url = lv_url.
  ENDMETHOD.
  METHOD to_xml.
    CALL TRANSFORMATION id
      SOURCE user = is_user
      RESULT XML rv_xml.
  ENDMETHOD.
  METHOD update.

    DATA: lv_xml TYPE string.

    lv_xml = to_xml( ms_user ).

    Lcl_abapgit_persistence_db=>get_instance( )->modify(
      iv_type  = Lcl_abapgit_persistence_db=>c_type_user
      iv_value = mv_user
      iv_data  = lv_xml ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD update_repo_config.

    DATA: lv_key  TYPE string.

    FIELD-SYMBOLS <ls_repo_config> TYPE ty_repo_config.

    lv_key  = to_lower( iv_url ).

    READ TABLE ms_user-repo_config ASSIGNING <ls_repo_config> WITH KEY url = lv_key.
    IF sy-subrc IS NOT INITIAL.
      APPEND INITIAL LINE TO ms_user-repo_config ASSIGNING <ls_repo_config>.
    ENDIF.
    <ls_repo_config>     = is_repo_config.
    <ls_repo_config>-url = lv_key.

    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_changes_only.

    rv_changes_only = ms_user-changes_only.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_default_git_user_email.

    rv_email = ms_user-default_git_user-email.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_default_git_user_name.

    rv_username = ms_user-default_git_user-name.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_diff_first.
    rv_diff_first = ms_user-diff_first.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_diff_unified.

    rv_diff_unified = ms_user-diff_unified.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_favorites.

    rt_favorites = ms_user-favorites.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_hide_files.

    rv_hide = ms_user-hide_files.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_order_by.
    rv_order_by = ms_user-order_by.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_order_descending.
    rv_order_descending = ms_user-order_descending.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_git_user_email.

    rv_email = read_repo_config( iv_url )-git_user-email.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_git_user_name.

    rv_username = read_repo_config( iv_url )-git_user-name.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_last_change_seen.

    rv_version = read_repo_config( iv_url )-last_change_seen.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_login.

    rv_login = read_repo_config( iv_url )-login.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_repo_show.

    rv_key = ms_user-repo_show.

    IF rv_key IS INITIAL.
      RETURN.
    ENDIF.

    " Check if repo exists
    TRY.
        Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = Lcl_abapgit_persistence_db=>c_type_repo
          iv_value = rv_key ).
      CATCH Lcx_abapgit_not_found.
        " remove invalid key
        CLEAR rv_key.
        Lif_abapgit_persist_user~set_repo_show( rv_key ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_settings.

    rs_user_settings = ms_user-settings.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_show_folders.

    rv_folders = ms_user-show_folders.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~is_favorite_repo.

    READ TABLE ms_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    rv_yes = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_default_git_user_email.

    ms_user-default_git_user-email = iv_email.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_default_git_user_name.

    ms_user-default_git_user-name = iv_username.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_diff_first.
    ms_user-diff_first = iv_diff_first.
    update( ).
    rv_diff_first = ms_user-diff_first.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_order_by.
    ms_user-order_by = iv_order_by.
    update( ).
    rv_order_by = ms_user-order_by.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_order_descending.
    ms_user-order_descending = iv_order_descending.
    update( ).
    rv_order_descending = ms_user-order_descending.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_git_user_email.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                = read_repo_config( iv_url ).
    ls_repo_config-git_user-email = iv_email.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_git_user_name.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config               = read_repo_config( iv_url ).
    ls_repo_config-git_user-name = iv_username.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_last_change_seen.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config                  = read_repo_config( iv_url ).
    ls_repo_config-last_change_seen = iv_version.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_login.

    DATA: ls_repo_config TYPE ty_repo_config.

    ls_repo_config       = read_repo_config( iv_url ).
    ls_repo_config-login = iv_login.
    update_repo_config( iv_url = iv_url
                        is_repo_config = ls_repo_config ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_repo_show.

    ms_user-repo_show = iv_key.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_settings.

    ms_user-settings = is_user_settings.
    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_changes_only.

    ms_user-changes_only = boolc( ms_user-changes_only = abap_false ).
    update( ).

    rv_changes_only = ms_user-changes_only.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_diff_unified.

    ms_user-diff_unified = boolc( ms_user-diff_unified = abap_false ).
    update( ).

    rv_diff_unified = ms_user-diff_unified.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_favorite.

    READ TABLE ms_user-favorites TRANSPORTING NO FIELDS
      WITH KEY table_line = iv_repo_key.

    IF sy-subrc = 0.
      DELETE ms_user-favorites INDEX sy-tabix.
    ELSE.
      APPEND iv_repo_key TO ms_user-favorites.
    ENDIF.

    update( ).

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_hide_files.

    ms_user-hide_files = boolc( ms_user-hide_files = abap_false ).
    update( ).

    rv_hide = ms_user-hide_files.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~toggle_show_folders.
    ms_user-show_folders = boolc( ms_user-show_folders = abap_false ).
    update( ).

    rv_folders = ms_user-show_folders.
  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~get_list_settings.

    rs_list_settings = ms_user-list_settings.

    IF rs_list_settings IS INITIAL.
      " for performance reasons, set "only favorites" as a default
      IF Lcl_abapgit_repo_srv=>get_instance( )->list_favorites( ) IS NOT INITIAL.
        rs_list_settings-only_favorites = abap_true.
      ENDIF.

      rs_list_settings-order_by = |NAME|.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_persist_user~set_list_settings.
    ms_user-list_settings = is_list_settings.
    update( ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSISTENCE_USER implementation

*>>>>>>> ZCX_ABAPGIT_AJSON_ERROR <<<<<<<*

*"* macro definitions
*include zcx_abapgit_ajson_error=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_ajson_error=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcx_abapgit_ajson_error=======ccau.


class LCX_ABAPGIT_AJSON_ERROR implementation.
*"* method's implementations
*include methods.
method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->RC = RC .
me->MESSAGE = MESSAGE .
me->LOCATION = LOCATION .
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_AJSON_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.
method raise.

  data lx type ref to Lcx_abapgit_ajson_error.

  create object lx exporting message = iv_msg.
  lx->set_location(
    iv_location = iv_location
    is_node     = is_node ).
  raise exception lx.

endmethod.
method set_location.

  data ls_msg type ty_message_parts.
  data lv_location type string.
  data lv_tmp type string.
  field-symbols <path> type string.
  field-symbols <name> type string.

  if iv_location is not initial.
    lv_location = iv_location.
  elseif is_node is not initial.
    assign component 'PATH' of structure is_node to <path>.
    assign component 'NAME' of structure is_node to <name>.
    if <path> is assigned and <name> is assigned.
      lv_location = <path> && <name>.
    endif.
  endif.

  if lv_location is not initial.
    lv_tmp = message && | @{ lv_location }|.
  else.
    lv_tmp = message.
  endif.

  ls_msg = lv_tmp.

  location = lv_location.
  a1       = ls_msg-a1.
  a2       = ls_msg-a2.
  a3       = ls_msg-a3.
  a4       = ls_msg-a4.

endmethod.
endclass. "ZCX_ABAPGIT_AJSON_ERROR implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_CONFIG_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_config_downlccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_config_downlccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_CONFIG_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    DATA: lv_partyp TYPE string.

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.

    set_attributes_to_template( ).
    ecatt_config ?= ecatt_object.

    CALL METHOD ('SET_ECATT_OBJECTS_TO_TEMPLATE'). " doesn't exist in 702

* MS180406
    set_var_mode_to_dom( ).
* ENDMS180406
    get_general_params_data( im_params = ecatt_config->params
                             im_ptyp   = lv_partyp ).
    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( ecatt_config->params ).
        set_deep_data_to_dom( im_params = ecatt_config->params
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

    set_variants_to_dom( ecatt_config->params ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_CONFIG_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_DATA_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_data_downl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_data_downl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_DATA_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    DATA: lv_partyp TYPE string.

    load_help = im_load_help.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    typ = im_object_type.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.

    ecatt_data ?= ecatt_object.
    set_attributes_to_template( ).
    get_general_params_data( im_params = ecatt_data->params
                             im_ptyp   = lv_partyp ).

    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( ecatt_data->params ).
        set_deep_data_to_dom( im_params = ecatt_data->params
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

* MS180406
    set_var_mode_to_dom( ).
* ENDMS180406
    set_variants_to_dom( ecatt_data->params ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_DATA_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_DATA_UPLOAD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_data_upload=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_data_upload=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_DATA_UPLOAD implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " donwnpoort from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
  METHOD on_ev_object_saved.
    DATA lo_ecatt_td TYPE REF TO cl_apl_ecatt_test_data.

    " Trickery to remove any local variants that do not exist on the remote on pull.

    SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.

    TRY.
        IF ex_ecatt_object->object_type <> ms_current_object-s_obj_type OR
           ex_ecatt_object->object_name <> ms_current_object-d_obj_name OR
           ex_ecatt_object->object_version <> ms_current_object-d_obj_ver.
          CREATE OBJECT mx_ecatt_apl
            EXPORTING
              textid    = cx_ecatt_apl=>any_text
              free_text = 'Unexpected object in save sequence'.
          RETURN.
        ENDIF.

        lo_ecatt_td ?= ex_ecatt_object.
        lo_ecatt_td->params->delete_variants( '*' ).
        TRY.
            CALL METHOD ('GET_VARIANTS_FROM_DOM_NEW')
              EXPORTING
                im_params = lo_ecatt_td->params.
          CATCH cx_sy_dyn_call_error.
            get_variants_from_dom( lo_ecatt_td->params ).
        ENDTRY.
        lo_ecatt_td->save( ).
      CATCH cx_ecatt_apl INTO mx_ecatt_apl.
        RETURN.
    ENDTRY.
  ENDMETHOD.
  METHOD upload.
    SET HANDLER on_ev_object_saved FOR ALL INSTANCES.

    ms_current_object-s_obj_type = ch_object-s_obj_type.
    ms_current_object-d_obj_name = ch_object-d_obj_name.
    ms_current_object-d_obj_ver = ch_object-d_obj_ver.

    TRY.
        super->upload( CHANGING ch_object = ch_object ).
        SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.
      CLEANUP.
        SET HANDLER on_ev_object_saved FOR ALL INSTANCES ACTIVATION abap_false.
    ENDTRY.

    IF mx_ecatt_apl IS BOUND.
      raise_upload_exception( previous = mx_ecatt_apl ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_DATA_UPLOAD implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SCRIPT_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_script_downlccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_script_downlccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SCRIPT_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    toolname = ecatt_object->attrib->get_tool_name( ).
    set_attributes_to_template( ).

    IF toolname = cl_apl_ecatt_const=>toolname_ecatt.

      ecatt_script ?= ecatt_object.

      set_script_to_template( ).

      TRY.
          get_general_params_data( ecatt_script->params ).
        CATCH cx_ecatt_apl.                              "#EC NO_HANDLER
*         proceed with download and report errors later
      ENDTRY.

      LOOP AT parm INTO wa_parm.
        TRY.
            IF wa_parm-value = '<INITIAL>'.
              CLEAR wa_parm-value.
            ENDIF.
            set_general_params_data_to_dom( ).
            IF NOT wa_parm-pstruc_typ IS INITIAL.
              set_deep_stru_to_dom( ecatt_script->params ).
              set_deep_data_to_dom( ecatt_script->params ).
              IF wa_parm-xmlref_typ = cl_apl_ecatt_const=>ref_type_c_tcd.
                set_control_data_for_tcd( is_param  = wa_parm
                                          io_params = ecatt_script->params ).

              ENDIF.
            ENDIF.
          CATCH cx_ecatt_apl.                            "#EC NO_HANDLER
*         proceed with download and report errors later
        ENDTRY.
      ENDLOOP.

    ELSE.

      set_blob_to_template( ).
      set_artmp_to_template( ).

    ENDIF.

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD escape_control_data.

    " Downport

    DATA: li_iter     TYPE REF TO if_ixml_node_iterator,
          li_textit   TYPE REF TO if_ixml_node_iterator,
          li_abapctrl TYPE REF TO if_ixml_node_collection,
          li_text     TYPE REF TO if_ixml_text,
          li_filter   TYPE REF TO if_ixml_node_filter,
          li_list     TYPE REF TO if_ixml_node_list,
          lv_value    TYPE etdom_name,
          li_vars     TYPE REF TO if_ixml_element,
          li_elem     TYPE REF TO if_ixml_element.

    li_vars = ii_element->find_from_name_ns( iv_tabname ).
    li_filter = ii_element->create_filter_node_type( if_ixml_node=>co_node_text ).
    IF li_vars IS NOT INITIAL.
      li_abapctrl = ii_element->get_elements_by_tag_name_ns( iv_node ).

* just for debugging
      li_iter = li_abapctrl->create_iterator( ).
      li_elem ?= li_iter->get_next( ).
      WHILE li_elem IS NOT INITIAL.
        li_list = li_elem->get_children( ).

        li_textit = li_list->create_rev_iterator_filtered( li_filter ).
        li_text ?= li_textit->get_next( ).
        IF li_text IS NOT INITIAL.
          lv_value = li_text->get_data( ).
          IF lv_value(1) = cl_abap_char_utilities=>minchar.
            REPLACE SECTION OFFSET 0 LENGTH 1 OF lv_value WITH space.
            li_text->set_value( value = lv_value ).
          ENDIF.
        ENDIF.
        CLEAR: li_textit, li_list, li_elem, lv_value.
        li_elem ?= li_iter->get_next( ).
      ENDWHILE.
      CLEAR: li_abapctrl, li_elem, li_iter.

    ENDIF.

  ENDMETHOD.
  METHOD set_artmp_to_template.

    " Downport

    DATA: li_artmp_node   TYPE REF TO if_ixml_element,
          lv_rc           TYPE sy-subrc,
          lv_text         TYPE string,
          lv_rc_args_tmpl TYPE i,
          lv_errmsg       TYPE string.

    li_artmp_node = template_over_all->create_simple_element(
                      name   = 'ECET_ARTMP'
                      parent = root_node ).

    ecatt_extprog->get_args_tmpl(
      IMPORTING
        ex_xml_arg_tmpl = lv_text
        ex_rc           = lv_rc_args_tmpl
        ex_errmsg       = lv_errmsg ).

    IF li_artmp_node IS INITIAL OR lv_rc_args_tmpl > 0.
      raise_download_exception(
          textid        = cx_ecatt_apl_util=>download_processing
          previous      = ex_ecatt
          called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_ARTMP_TO_TEMPLATE'
          free_text     = lv_errmsg ).
    ENDIF.

    lv_rc = li_artmp_node->set_value( lv_text ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_ARTMP_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD set_blob_to_template.

    " Downport

    DATA: li_blob_node TYPE REF TO if_ixml_element,
          lv_rc        TYPE sy-subrc,
          lv_text      TYPE string.

    li_blob_node = template_over_all->create_simple_element(
                  name   = 'ECET_BLOBS'
                  parent = root_node ).

    IF li_blob_node IS INITIAL.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_BLOB_TO_TEMPLATE' ).
    ENDIF.

    ecatt_extprog->get_blob(
      EXPORTING
        im_whole_data = 1
      IMPORTING
        ex_xml_blob   = lv_text ).

    lv_rc = li_blob_node->set_value( lv_text ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_BLOB_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD set_control_data_for_tcd.

    " Downport

    DATA: lt_params TYPE ettcd_params_tabtype,
          lt_verbs  TYPE ettcd_verbs_tabtype,
          lt_vars   TYPE ettcd_vars_tabtype,
          lt_dp_tab TYPE ettcd_dp_tab_tabtype,
          lt_dp_for TYPE ettcd_dp_for_tabtype,
          lt_dp_pro TYPE ettcd_dp_pro_tabtype,
          lt_dp_fld TYPE ettcd_dp_fld_tabtype,
          lt_svars  TYPE ettcd_svars_tabtype.

    DATA: li_element   TYPE REF TO if_ixml_element,
          li_deep_tcd  TYPE REF TO if_ixml_element,
          lv_rc        TYPE sy-subrc,
          lv_name      TYPE string,
          lv_parname   TYPE string,
          lo_pval_xml  TYPE REF TO cl_apl_ecatt_xml_data,
          lo_ctrl_tabs TYPE REF TO cl_apl_ecatt_control_tables.

    FIELD-SYMBOLS: <lt_tab> TYPE STANDARD TABLE.

    IF is_param-xmlref_typ <> cl_apl_ecatt_const=>ref_type_c_tcd OR io_params IS INITIAL.
      RETURN.
    ENDIF.

    lv_parname = is_param-pname.

    io_params->get_param_value(     "TCD command interface
      EXPORTING
        im_var_id   = cl_apl_ecatt_const=>varid_default_val
        im_pname    = lv_parname
        im_pindex   = is_param-pindex
      IMPORTING
        ex_pval_xml = lo_pval_xml ).

    lo_ctrl_tabs = lo_pval_xml->get_control_tables_ref( ).
    IF lo_ctrl_tabs IS INITIAL.
      RETURN.
    ENDIF.

    lo_ctrl_tabs->get_control_tables(          "Read 8 control tables
      IMPORTING
        ex_params = lt_params
        ex_verbs  = lt_verbs
        ex_vars   = lt_vars
        ex_dp_tab = lt_dp_tab
        ex_dp_for = lt_dp_for
        ex_dp_pro = lt_dp_pro
        ex_dp_fld = lt_dp_fld
        ex_svars  = lt_svars ).

    IF lt_params IS INITIAL OR
       lt_verbs  IS INITIAL OR
       lt_vars   IS INITIAL OR
       lt_dp_tab IS INITIAL OR
       lt_dp_for IS INITIAL OR
       lt_dp_pro IS INITIAL OR
       lt_dp_fld IS INITIAL OR
       lt_svars  IS INITIAL.

      RETURN.
    ENDIF.

    li_deep_tcd = template_over_all->create_simple_element_ns(
                    name   = cl_apl_xml_const=>upl_tcd_node
                    parent = ap_current_param ).

    IF li_deep_tcd IS INITIAL.
      raise_download_exception(
            textid   = cx_ecatt_apl_util=>download_processing
            previous = ex_ecatt ).
    ENDIF.

    DO 8 TIMES.                                "Loop at 8 control tables
      CASE sy-index.
        WHEN 1.
          lv_name = 'ETTCD_PARAMS_TABTYPE'.
          ASSIGN lt_params TO <lt_tab>.
        WHEN 2.
          lv_name = 'ETTCD_VERBS_TABTYPE'.
          ASSIGN lt_verbs TO <lt_tab>.
        WHEN 3.
          lv_name = 'ETTCD_VARS_TABTYPE'.
          ASSIGN lt_vars TO <lt_tab>.
        WHEN 4.
          lv_name = 'ETTCD_DP_TAB_TABTYPE'.
          ASSIGN lt_dp_tab TO <lt_tab>.
        WHEN 5.
          lv_name = 'ETTCD_DP_FOR_TABTYPE'.
          ASSIGN lt_dp_for TO <lt_tab>.
        WHEN 6.
          lv_name = 'ETTCD_DP_PRO_TABTYPE'.
          ASSIGN lt_dp_pro TO <lt_tab>.
        WHEN 7.
          lv_name = 'ETTCD_DP_FLD_TABTYPE'.
          ASSIGN lt_dp_fld TO <lt_tab>.
        WHEN 8.
          lv_name = 'ETTCD_SVARS_TABTYPE'.
          ASSIGN lt_svars TO <lt_tab>.
      ENDCASE.

      CALL FUNCTION 'SDIXML_DATA_TO_DOM'       "Ast generieren lassen
        EXPORTING
          name         = lv_name
          dataobject   = <lt_tab>
        IMPORTING
          data_as_dom  = li_element
        EXCEPTIONS
          illegal_name = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        raise_download_exception(
              textid   = cx_ecatt_apl_util=>download_processing
              previous = ex_ecatt ).
      ENDIF.

* Ast in Hauptbaum haengen
      lv_rc = li_deep_tcd->append_child( li_element ).

      IF lv_rc <> 0.
        raise_download_exception(
              textid   = cx_ecatt_apl_util=>download_processing
              previous = ex_ecatt ).
      ENDIF.
      FREE li_element.
      UNASSIGN <lt_tab>.
    ENDDO.

    escape_control_data( ii_element = li_deep_tcd
      iv_tabname = 'ETTCD_VARS_TABTYPE'
      iv_node    = 'CB_INDEX' ).

    escape_control_data(
      ii_element = li_deep_tcd
      iv_tabname = 'ETTCD_VERBS_TABTYPE'
      iv_node    = 'NAME' ).

    FREE: lt_dp_tab, lt_dp_for, lt_dp_fld, lt_svars,
          lt_params, lt_vars,   lt_dp_pro, lt_verbs.

  ENDMETHOD.
  METHOD set_script_to_template.

    " Downport

    DATA:
      lt_text    TYPE etxml_line_tabtype,
      li_element TYPE REF TO if_ixml_element,
      lv_rc      TYPE sy-subrc.

    ecatt_script->get_script_text( CHANGING scripttext = lt_text ).

    mi_script_node = template_over_all->create_simple_element(
                        name = 'SCRIPT'
                        parent = root_node ).

    IF mi_script_node IS INITIAL.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).
    ENDIF.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ETXML_LINE_TABTYPE'
        dataobject   = lt_text
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).

    ENDIF.

    lv_rc = mi_script_node->append_child( li_element ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SCRIPT_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SP_DOWNLOAD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_sp_download=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_sp_download=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SP_DOWNLOAD implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " We inherit from CL_APL_ECATT_DOWNLOAD because CL_APL_ECATT_SP_DOWNLOAD
    " doesn't exist in 702

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    set_attributes_to_template( ).

    set_sp_data_to_template( ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD set_sp_data_to_template.

    " downport

    DATA: li_dom                     TYPE REF TO if_ixml_document,
          li_start_profile_data_node TYPE REF TO if_ixml_element,
          li_element                 TYPE REF TO if_ixml_element,
          lv_sp_xml                  TYPE etxml_line_str,
          lo_ecatt_sp                TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_object> TYPE data.

    li_start_profile_data_node = template_over_all->create_simple_element(
                                   name = 'START_PROFILE'
                                   parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_object>.
    ASSERT sy-subrc = 0.

    lo_ecatt_sp = <lg_ecatt_object>.

    TRY.
        CALL METHOD lo_ecatt_sp->('GET_SP_ATTRIBUTES')
          IMPORTING
            e_sp_xml = lv_sp_xml.
      CATCH cx_ecatt_apl.
    ENDTRY.

    CALL FUNCTION 'SDIXML_XML_TO_DOM'
      EXPORTING
        xml      = lv_sp_xml
      IMPORTING
        document = li_dom.

    li_element = li_dom->get_root_element( ).
    li_start_profile_data_node->append_child( li_element ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SP_DOWNLOAD implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SP_UPLOAD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_sp_upload===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_sp_upload===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SP_UPLOAD implementation.
*"* method's implementations
*include methods.
  METHOD get_ecatt_sp.

    " downport

    DATA: li_ixml               TYPE REF TO if_ixml,
          li_section            TYPE REF TO if_ixml_element,
          li_dom                TYPE REF TO if_ixml_document,
          li_root               TYPE REF TO if_ixml_node,
          lv_start_profile      TYPE etxml_line_str,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_sp           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_object> TYPE any.

    TRY.
        li_section = template_over_all->find_from_name_ns( 'START_PROFILE' ).

        IF NOT li_section IS INITIAL.
          li_ixml = cl_ixml=>create( ).
          li_dom  = li_ixml->create_document( ).
          li_root ?= li_section->clone( ).
          li_dom->append_child( li_root ).
          CALL FUNCTION 'SDIXML_DOM_TO_XML'
            EXPORTING
              document      = li_dom
            IMPORTING
              xml_as_string = lv_start_profile.

          ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_object>.
          ASSERT sy-subrc = 0.

          lo_ecatt_sp = <lg_ecatt_object>.

          CALL METHOD lo_ecatt_sp->('SET_SP_ATTRIBUTES')
            EXPORTING
              i_sp_xml = lv_start_profile.

        ENDIF.
      CATCH cx_ecatt_apl.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.
  ENDMETHOD.
  METHOD upload.

    " We inherit from CL_APL_ECATT_UPLOAD because CL_APL_ECATT_SP_UPLOAD
    " doesn't exist in 702

    " Downport

    "26.03.2013

    DATA: lx_ecatt    TYPE REF TO cx_ecatt_apl,
          lv_exists   TYPE etonoff,
          lv_exc_occ  TYPE etonoff,
          ls_tadir    TYPE tadir,
          lo_ecatt_sp TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_sp> TYPE any,
                   <lg_d_akh>    TYPE data,
                   <lg_i_akh>    TYPE data.

    TRY.
        ch_object-i_devclass = ch_object-d_devclass.

        ASSIGN COMPONENT 'D_AKH' OF STRUCTURE ch_object
               TO <lg_d_akh>. " doesn't exist in 702
        ASSIGN COMPONENT 'I_AKH' OF STRUCTURE ch_object
               TO <lg_i_akh>. " doesn't exist in 702
        IF <lg_d_akh> IS ASSIGNED AND <lg_i_akh> IS ASSIGNED.
          <lg_i_akh> = <lg_d_akh>.
        ENDIF.

        super->upload( CHANGING ch_object = ch_object ).

        upload_data_from_stream( ch_object-filename ).

      CATCH cx_ecatt_apl INTO lx_ecatt.
        IF template_over_all IS INITIAL.
          RAISE EXCEPTION lx_ecatt.
        ELSE.
          lv_exc_occ = 'X'.
        ENDIF.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_ATTRIBUTES_FROM_DOM_NEW') " doesn't exist in 720
          CHANGING
            ch_object = ch_object.
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.

    ASSIGN ecatt_object TO <lg_ecatt_sp>.
    ASSERT sy-subrc = 0.

    lo_ecatt_sp = <lg_ecatt_sp>.

    TRY.
        get_ecatt_sp( ).
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        lv_exists = cl_apl_ecatt_object=>existence_check_object(
                      im_name               = ch_object-d_obj_name
                      im_version            = ch_object-d_obj_ver
                      im_obj_type           = ch_object-s_obj_type
                      im_exists_any_version = 'X' ).

        IF lv_exists = space.
          CALL METHOD lo_ecatt_sp->('SET_TADIR_FOR_NEW_OBJECT')
            EXPORTING
              im_tadir_for_new_object = tadir_preset.
        ENDIF.
      CATCH cx_ecatt.
        CLEAR lv_exists.
    ENDTRY.

    TRY.
        CALL METHOD lo_ecatt_sp->('SAVE')
          EXPORTING
            im_do_commit = 'X'.
      CATCH cx_ecatt_apl INTO lx_ecatt.
        lv_exc_occ = 'X'.
    ENDTRY.
* Devesh,C5129871  18.07.2011  Releasing enqueu after uploading
*begin
    TRY.
        ecatt_object->close_object( im_suppress_events = 'X' ).
      CATCH cx_ecatt_apl INTO lx_ecatt.
    ENDTRY.
*end
*     get devclass from existing object
    TRY.
        cl_apl_ecatt_object=>get_tadir_entry(
          EXPORTING im_obj_name = ch_object-d_obj_name
                    im_obj_type = ch_object-s_obj_type
          IMPORTING ex_tadir = ls_tadir ).

        ch_object-d_devclass = ls_tadir-devclass.

      CATCH cx_ecatt.
        CLEAR ls_tadir.
    ENDTRY.
    IF lv_exc_occ = 'X'.
      raise_upload_exception( previous = lx_ecatt ).
    ENDIF.

  ENDMETHOD.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_APL_ECATT_START_PROFIL SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SP_UPLOAD implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SYSTEM_DOWNL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_system_downlccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_system_downlccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SYSTEM_DOWNL implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    set_attributes_to_template( ).
    set_systems_data_to_template( ).
    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD set_systems_data_to_template.

    DATA: lo_ecatt_systems TYPE REF TO cl_apl_ecatt_system_data,
          lt_sys_data      TYPE etsys_def_tabtype,
          ls_sys_data      TYPE etsys_def,
          li_item          TYPE REF TO if_ixml_element,
          li_sysdata_node  TYPE REF TO if_ixml_element.

    lo_ecatt_systems ?= ecatt_object.
    lt_sys_data = lo_ecatt_systems->get_system_data( ).

    li_sysdata_node = template_over_all->create_simple_element(
                        name = 'SYSTEMS_DATA'
                        parent = root_node ).

    etpar_node = template_over_all->create_simple_element(
                   name = 'ETSYS_DEF'
                   parent = li_sysdata_node ).

    LOOP AT lt_sys_data INTO ls_sys_data.

      CLEAR: ls_sys_data-sys_desc, ls_sys_data-instance.

      CALL FUNCTION 'SDIXML_DATA_TO_DOM'
        EXPORTING
          name         = 'item'
          dataobject   = ls_sys_data
        IMPORTING
          data_as_dom  = li_item
        CHANGING
          document     = template_over_all
        EXCEPTIONS
          illegal_name = 1
          OTHERS       = 2.
      ASSERT sy-subrc = 0.

      etpar_node->append_child( li_item ).

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SYSTEM_DOWNL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_SYSTEM_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_system_upl==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_system_upl==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_SYSTEM_UPL implementation.
*"* method's implementations
*include methods.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_APL_ECATT_SYSTEMS_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_SYSTEM_UPL implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_VAL_OBJ_DOWN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_val_obj_downccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_val_obj_downccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_VAL_OBJ_DOWN implementation.
*"* method's implementations
*include methods.
  METHOD download.

    " We inherit from CL_APL_ECATT_DOWNLOAD because CL_APL_ECATT_VO_DOWNLOAD
    " doesn't exist in 702

    " Downport

    DATA: lv_partyp   TYPE string,
          lo_ecatt_vo TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any,
                   <lo_params>   TYPE REF TO cl_apl_ecatt_params.

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    lv_partyp = cl_apl_ecatt_const=>params_type_par.


    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    set_attributes_to_template( ).
    set_ecatt_impl_detail( ).
    set_ecatt_flags( ).
    set_business_msgs( ).

    ASSIGN lo_ecatt_vo->('PARAMS')
           TO <lo_params>.
    ASSERT sy-subrc = 0.

    get_general_params_data( im_params = <lo_params>
                             im_ptyp   = lv_partyp ).
    LOOP AT parm INTO wa_parm.
      set_general_params_data_to_dom( ).
      IF NOT wa_parm-val_type IS INITIAL.
        set_deep_stru_to_dom( <lo_params> ).
        set_deep_data_to_dom( im_params = <lo_params>
                              im_pindex = wa_parm-pindex ).
      ENDIF.
    ENDLOOP.

    set_variants_to_dom( <lo_params> ).

    download_data( ).

  ENDMETHOD.
  METHOD download_data.

    " Downport

    mv_xml_stream = Lcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.
  METHOD set_business_msgs.

    DATA:
      lt_buss_msg_ref   TYPE Lif_abapgit_ecatt=>ty_bus_msgs,
      li_element        TYPE REF TO if_ixml_element,
      li_insert_objects TYPE REF TO if_ixml_element,
      lo_ecatt_vo       TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    mi_objects_node = template_over_all->create_simple_element(
                                           name   = 'BUSINESS_MESSAGES'
                                           parent = root_node ).

    CALL METHOD lo_ecatt_vo->('GET_BUSSINESS_MSG')
      IMPORTING
        ex_buss_msg_ref = lt_buss_msg_ref.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ETVO_MSG'
        dataobject   = lt_buss_msg_ref
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects ?= template_over_all->find_from_name( 'BUSINESS_MESSAGES' ).

    li_insert_objects->append_child( li_element ).

  ENDMETHOD.
  METHOD set_ecatt_flags.

    DATA:
      lv_invert_validation TYPE Lif_abapgit_ecatt=>ty_invert_validation,
      lv_error_prio        TYPE Lif_abapgit_ecatt=>ty_error_prio,
      li_element           TYPE REF TO if_ixml_element,
      li_insert_objects    TYPE REF TO if_ixml_element,
      lo_ecatt_vo          TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    mi_objects_node = template_over_all->create_simple_element(
                                           name   = 'VO_FLAGS'
                                           parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    CALL METHOD lo_ecatt_vo->('GET_INVERT_VALIDATION_FLAG')
      RECEIVING
        re_invert_validation = lv_invert_validation.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'INVERT_VALIDATION'
        dataobject   = lv_invert_validation
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects ?= template_over_all->find_from_name( 'VO_FLAGS' ).

    li_insert_objects->append_child( li_element ).

    CALL METHOD lo_ecatt_vo->('GET_ERROR_PRIORITY')
      RECEIVING
        re_error_prio = lv_error_prio.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ERROR_PRIORITY'
        dataobject   = lv_error_prio
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects = template_over_all->find_from_name( 'VO_FLAGS' ).

    li_insert_objects->append_child( li_element ).

  ENDMETHOD.
  METHOD set_ecatt_impl_detail.

    DATA:
      ls_impl_details   TYPE Lif_abapgit_ecatt=>ty_impl_det,
      li_element        TYPE REF TO if_ixml_element,
      li_insert_objects TYPE REF TO if_ixml_element,
      lo_ecatt_vo       TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    mi_objects_node = template_over_all->create_simple_element(
                                           name   = 'IMPL_DETAILS'
                                           parent = root_node ).

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    CALL METHOD lo_ecatt_vo->('GET_IMPL_DETAILS')
      RECEIVING
        re_impl_details = ls_impl_details.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'IMPL_DET'
        dataobject   = ls_impl_details
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    li_insert_objects = template_over_all->find_from_name( 'IMPL_DETAILS' ).

    li_insert_objects->append_child( li_element ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_VAL_OBJ_DOWN implementation

*>>>>>>> ZCL_ABAPGIT_ECATT_VAL_OBJ_UPL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_ecatt_val_obj_upl=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_ecatt_val_obj_upl=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_ECATT_VAL_OBJ_UPL implementation.
*"* method's implementations
*include methods.
  METHOD get_business_msgs_from_dom.

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: li_section            TYPE REF TO if_ixml_element,
          lt_buss_msg_ref       TYPE Lif_abapgit_ecatt=>ty_bus_msgs,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_vo           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    li_section = template_over_all->find_from_name_ns( 'ETVO_MSG' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = lt_buss_msg_ref
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR lt_buss_msg_ref.
      ENDIF.
    ENDIF.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_BUSSINESS_MSG')
          EXPORTING
            im_buss_msg_ref = lt_buss_msg_ref.
      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.

  ENDMETHOD.
  METHOD get_impl_detail_from_dom.

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: li_section            TYPE REF TO if_ixml_element,
          ls_impl_details       TYPE Lif_abapgit_ecatt=>ty_impl_det,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_vo           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    li_section = template_over_all->find_from_name_ns( 'IMPL_DET' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = ls_impl_details
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR ls_impl_details.
      ENDIF.
    ENDIF.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_IMPL_DETAILS')
          EXPORTING
            im_impl_details = ls_impl_details.
      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.

  ENDMETHOD.
  METHOD get_vo_flags_from_dom.

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: li_section            TYPE REF TO if_ixml_element,
          lv_error_prio         TYPE Lif_abapgit_ecatt=>ty_error_prio,
          lv_invert_validation  TYPE Lif_abapgit_ecatt=>ty_invert_validation,
          lv_exception_occurred TYPE etonoff,
          lo_ecatt_vo           TYPE REF TO object.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any.

    li_section = template_over_all->find_from_name_ns( 'INVERT_VALIDATION' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = lv_invert_validation
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR lv_invert_validation.
      ENDIF.
    ENDIF.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_INVERT_VALIDATION_FLAG')
          EXPORTING
            im_invert_validation = lv_invert_validation.

      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    li_section = template_over_all->find_from_name_ns( 'ERROR_PRIORITY' ).

    IF NOT li_section IS INITIAL.
      CALL FUNCTION 'SDIXML_DOM_TO_DATA'
        EXPORTING
          data_as_dom    = li_section
        IMPORTING
          dataobject     = lv_error_prio
        EXCEPTIONS
          illegal_object = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
        CLEAR lv_invert_validation.
      ENDIF.
    ENDIF.

    TRY.
        CALL METHOD lo_ecatt_vo->('SET_ERROR_PRIORITY')
          EXPORTING
            im_error_prio = lv_error_prio.
      CATCH cx_ecatt_apl INTO exception_to_raise.
        lv_exception_occurred = 'X'.
    ENDTRY.

    IF lv_exception_occurred = 'X'.
      raise_upload_exception( previous = exception_to_raise ).
    ENDIF.

  ENDMETHOD.
  METHOD upload.

    " We inherit from CL_APL_ECATT_UPLOAD because CL_APL_ECATT_VO_UPLOAD
    " doesn't exist in 702

    " downport from CL_APL_ECATT_VO_UPLOAD

    DATA: lx_ex       TYPE REF TO cx_ecatt_apl,
          lv_exists   TYPE etonoff,
          lv_exc_occ  TYPE etonoff,
          ls_tadir    TYPE tadir,
          lo_ecatt_vo TYPE REF TO object,
          lo_params   TYPE REF TO cl_apl_ecatt_params.

    FIELD-SYMBOLS: <lg_ecatt_vo> TYPE any,
                   <lg_params>   TYPE data,
                   <lg_d_akh>    TYPE data,
                   <lg_i_akh>    TYPE data.

    TRY.
        ch_object-i_devclass = ch_object-d_devclass.

        ASSIGN COMPONENT 'D_AKH' OF STRUCTURE ch_object
               TO <lg_d_akh>. " doesn't exist in 702
        ASSIGN COMPONENT 'I_AKH' OF STRUCTURE ch_object
               TO <lg_i_akh>. " doesn't exist in 702
        IF <lg_d_akh> IS ASSIGNED AND <lg_i_akh> IS ASSIGNED.
          <lg_i_akh> = <lg_d_akh>.
        ENDIF.

        super->upload( CHANGING ch_object = ch_object ).

        upload_data_from_stream( ch_object-filename ).
      CATCH cx_ecatt_apl INTO lx_ex.
        IF template_over_all IS INITIAL.
          RAISE EXCEPTION lx_ex.
        ELSE.
          lv_exc_occ = 'X'.
        ENDIF.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_ATTRIBUTES_FROM_DOM_NEW') " doesn't exit in 702
          CHANGING
            ch_object = ch_object.
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    ASSIGN ('ECATT_OBJECT') TO <lg_ecatt_vo>.
    ASSERT sy-subrc = 0.

    lo_ecatt_vo = <lg_ecatt_vo>.

    ASSIGN lo_ecatt_vo->('PARAMS') TO <lg_params>.
    ASSERT sy-subrc = 0.

    lo_params = <lg_params>.

    TRY.
        get_impl_detail_from_dom( ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        get_vo_flags_from_dom( ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        get_business_msgs_from_dom( ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        CALL METHOD ('GET_PARAMS_FROM_DOM_NEW') " doesn't exist in 702
          EXPORTING
            im_params = lo_params.
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        get_variants_from_dom( lo_params ).
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

    TRY.
        lv_exists = cl_apl_ecatt_object=>existence_check_object(
                im_name               = ch_object-d_obj_name
                im_version            = ch_object-d_obj_ver
                im_obj_type           = ch_object-s_obj_type
                im_exists_any_version = 'X' ).

        IF lv_exists = space.
          CALL METHOD lo_ecatt_vo->('SET_TADIR_FOR_NEW_OBJECT')
            EXPORTING
              im_tadir_for_new_object = tadir_preset.
        ENDIF.
      CATCH cx_ecatt.
        CLEAR lv_exists.
    ENDTRY.

    TRY.
        CALL METHOD lo_ecatt_vo->('SAVE')
          EXPORTING
            im_do_commit = 'X'.
      CATCH cx_ecatt_apl INTO lx_ex.
        lv_exc_occ = 'X'.
    ENDTRY.

*     get devclass from existing object
    TRY.
        cl_apl_ecatt_object=>get_tadir_entry(
          EXPORTING im_obj_name = ch_object-d_obj_name
                    im_obj_type = ch_object-s_obj_type
          IMPORTING ex_tadir = ls_tadir ).

        ch_object-d_devclass = ls_tadir-devclass.

      CATCH cx_ecatt.
        CLEAR ls_tadir.
    ENDTRY.
    IF lv_exc_occ = 'X'.
      raise_upload_exception( previous = lx_ex ).
    ENDIF.

  ENDMETHOD.
  METHOD upload_data_from_stream.

    " Downport
    template_over_all = Lcl_abapgit_ecatt_helper=>upload_data_from_stream( mv_external_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_ecatt_upload~set_stream_for_upload.

    " downport from CL_ABAPGIT_ECATT_DATA_UPLOAD SET_STREAM_FOR_UPLOAD
    mv_external_xml = iv_xml.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_ECATT_VAL_OBJ_UPL implementation

*>>>>>>> ZCL_ABAPGIT_EXCEPTION_VIEWER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_exception_viewer==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_exception_viewer==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_EXCEPTION_VIEWER implementation.
*"* method's implementations
*include methods.
  METHOD add_row.

    DATA: lo_row TYPE REF TO cl_salv_form_layout_flow.

    lo_row = io_grid->add_row( ).

    lo_row->create_label( position = 1
                          text     = iv_col_1 ).

    lo_row->create_label( position = 2
                          text     = iv_col_2 ).

  ENDMETHOD.
  METHOD build_top_of_list.

    DATA: lo_grid TYPE REF TO cl_salv_form_layout_grid.

    CREATE OBJECT lo_grid
      EXPORTING
        columns = 2.

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Main program:'
             iv_col_2 = is_top_of_stack-mainprogram ).

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Include name:'
             iv_col_2 = is_top_of_stack-include ).

    add_row( io_grid  = lo_grid
             iv_col_1 = 'Source line'
             iv_col_2 = |{ is_top_of_stack-line }| ).

    ro_form = lo_grid.

  ENDMETHOD.
  METHOD constructor.

    mx_error = ix_error.
    mt_callstack = mx_error->mt_callstack.

  ENDMETHOD.
  METHOD extract_classname.

    rv_classname = substring_before( val   = iv_mainprogram
                                     regex = '=*CP$' ).

  ENDMETHOD.
  METHOD get_top_of_callstack.

    READ TABLE mt_callstack INDEX 1
                            INTO rs_top_of_callstack.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Callstack is empty| ).
    ENDIF.

  ENDMETHOD.
  METHOD goto_message.

    DATA: lt_bdcdata TYPE STANDARD TABLE OF bdcdata,
          ls_bdcdata LIKE LINE OF lt_bdcdata.

    ls_bdcdata-program  = 'SAPLWBMESSAGES'.
    ls_bdcdata-dynpro   = '0100'.
    ls_bdcdata-dynbegin = abap_true.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'RSDAG-ARBGB'.
    ls_bdcdata-fval = mx_error->if_t100_message~t100key-msgid.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'MSG_NUMMER'.
    ls_bdcdata-fval = mx_error->if_t100_message~t100key-msgno.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'RSDAG-MSGFLAG'.
    ls_bdcdata-fval = 'X'.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CLEAR: ls_bdcdata.
    ls_bdcdata-fnam = 'BDC_OKCODE'.
    ls_bdcdata-fval = '=WB_DISPLAY'.
    INSERT ls_bdcdata INTO TABLE lt_bdcdata.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                   = 'SE91'
        mode_val                = 'E'
      TABLES
        using_tab               = lt_bdcdata
      EXCEPTIONS
        call_transaction_denied = 1
        tcode_invalid           = 2
        OTHERS                  = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD goto_source.

    goto_source_code( get_top_of_callstack( ) ).

  ENDMETHOD.
  METHOD goto_source_code.

    CONSTANTS:
      BEGIN OF lc_obj_type,
        class   TYPE trobjtype VALUE `CLAS`,
        program TYPE trobjtype VALUE `PROG`,
      END OF lc_obj_type.

    DATA:
      ls_item      TYPE Lif_abapgit_definitions=>ty_item,
      ls_sub_item  TYPE Lif_abapgit_definitions=>ty_item,
      lv_classname LIKE ls_item-obj_name.

    " you should remember that we distinct two cases
    " 1) we navigate to a global class
    " 2) we navigate to a program
    " the latter one is the default case

    lv_classname = extract_classname( is_callstack-mainprogram ).

    IF lv_classname IS NOT INITIAL.
      ls_item-obj_name = lv_classname.
      ls_item-obj_type = lc_obj_type-class.
    ELSE.
      ls_item-obj_name = is_callstack-mainprogram.
      ls_item-obj_type = lc_obj_type-program.
    ENDIF.

    ls_sub_item-obj_name = is_callstack-include.
    ls_sub_item-obj_type = lc_obj_type-program.

    Lcl_abapgit_objects=>jump(
      is_item        = ls_item
      is_sub_item    = ls_sub_item
      iv_line_number = is_callstack-line ).

  ENDMETHOD.
  METHOD on_double_click.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.
    FIELD-SYMBOLS: <ls_callstack> TYPE abap_callstack_line.

    READ TABLE mt_callstack ASSIGNING <ls_callstack>
                            INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        goto_source_code( <ls_callstack> ).

      CATCH Lcx_abapgit_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
  METHOD set_text.

    DATA: lo_column      TYPE REF TO cl_salv_column,
          lv_short_text  TYPE scrtext_s,
          lv_medium_text TYPE scrtext_m,
          lv_long_text   TYPE scrtext_l.

    lo_column = io_columns->get_column( iv_column ).

    lv_short_text  = iv_text.
    lv_medium_text = iv_text.
    lv_long_text   = iv_text.

    lo_column->set_short_text( lv_short_text ).
    lo_column->set_medium_text( lv_medium_text ).
    lo_column->set_long_text( lv_long_text ).

  ENDMETHOD.
  METHOD show_callstack.

    DATA: lx_error   TYPE REF TO cx_static_check,
          lo_event   TYPE REF TO cl_salv_events_table,
          lo_columns TYPE REF TO cl_salv_columns_table,
          lo_alv     TYPE REF TO cl_salv_table.
    DATA ls_position TYPE Lif_abapgit_popups=>ty_popup_position.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_callstack ).

        lo_alv->get_columns( )->set_optimize( ).

        lo_alv->set_top_of_list( build_top_of_list( get_top_of_callstack( ) ) ).

        ls_position = Lcl_abapgit_popups=>center(
          iv_width  = 150
          iv_height = 25 ).

        lo_alv->set_screen_popup( start_column = ls_position-start_column
                                  end_column   = ls_position-end_column
                                  start_line   = ls_position-start_row
                                  end_line     = ls_position-end_row ).

        lo_event = lo_alv->get_event( ).

        lo_columns = lo_alv->get_columns( ).

        set_text( io_columns = lo_columns
                  iv_column  = |LINE|
                  iv_text    = |Line| ).

        set_text( io_columns = lo_columns
                  iv_column  = |LINE|
                  iv_text    = |Line| ).

        set_text( io_columns = lo_columns
                  iv_column  = |BLOCKTYPE|
                  iv_text    = |Event Type| ).

        set_text( io_columns = lo_columns
                  iv_column  = |BLOCKNAME|
                  iv_text    = |Event| ).

        set_text( io_columns = lo_columns
                  iv_column  = |FLAG_SYSTEM|
                  iv_text    = |System| ).

        SET HANDLER on_double_click FOR lo_event.

        lo_alv->display( ).

      CATCH cx_static_check INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_EXCEPTION_VIEWER implementation

*>>>>>>> ZCL_ABAPGIT_FUNCTION_MODULE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_function_module===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_function_module===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_FUNCTION_MODULE implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_function_module~function_exists.

    DATA: lv_function_module_name TYPE c LENGTH 30.

    lv_function_module_name = iv_function_module_name.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lv_function_module_name
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    rv_exists = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FUNCTION_MODULE implementation

*>>>>>>> ZCL_ABAPGIT_GIT_BRANCH_LIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_branch_list===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_branch_list===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_branch_list===ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWXXSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_branch_list DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWXXSFY.




class LCL_ABAPGIT_GIT_BRANCH_LIST implementation.
*"* method's implementations
*include methods.
  METHOD complete_heads_branch_name.
    IF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-heads.
      rv_name = iv_branch_name.
    ELSE.
      rv_name = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_branch_name.
    ENDIF.
  ENDMETHOD.
  METHOD constructor.

    parse_branch_list(
      EXPORTING
        iv_data        = iv_data
      IMPORTING
        et_list        = mt_branches
        ev_head_symref = mv_head_symref ).

  ENDMETHOD.
  METHOD find_by_name.

    IF iv_branch_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Branch name empty' ).
    ENDIF.

    IF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      rs_branch = find_tag_by_name( iv_branch_name ).
    ELSE.

      READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = iv_branch_name.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Branch { get_display_name( iv_branch_name )
          } not found. Use 'Branch' > 'Switch' to select a different branch| ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD find_tag_by_name.

    READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = Lcl_abapgit_git_tag=>add_peel( iv_branch_name ).
    IF sy-subrc <> 0.

      READ TABLE mt_branches INTO rs_branch
        WITH TABLE KEY name_key
        COMPONENTS name = iv_branch_name.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Branch not found' ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD get_all.

    rt_branches = mt_branches.

  ENDMETHOD.
  METHOD get_branches_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-type = Lif_abapgit_git_definitions=>c_git_branch_type-branch.
        APPEND <ls_branch> TO rt_branches.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_display_name.
    rv_display_name = iv_branch_name.

    IF rv_display_name CP Lif_abapgit_git_definitions=>c_git_branch-heads.
      REPLACE FIRST OCCURRENCE OF Lif_abapgit_git_definitions=>c_git_branch-heads_prefix IN rv_display_name WITH ''.
    ELSEIF rv_display_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      rv_display_name = Lcl_abapgit_git_tag=>remove_tag_prefix( Lcl_abapgit_git_tag=>remove_peel( rv_display_name ) ).
    ENDIF.

  ENDMETHOD.
  METHOD get_head_symref.
    rv_head_symref = mv_head_symref.
  ENDMETHOD.
  METHOD get_tags_only.
    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    LOOP AT mt_branches ASSIGNING <ls_branch>
        WHERE type = Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag
        OR type = Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
      APPEND <ls_branch> TO rt_tags.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_type.

    FIELD-SYMBOLS: <lv_result> TYPE LINE OF string_table.

    rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-other.

    IF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-heads OR
       iv_branch_name = Lif_abapgit_git_definitions=>c_head_name.
      rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-branch.

    ELSEIF iv_branch_name CP Lif_abapgit_git_definitions=>c_git_branch-tags.

      READ TABLE it_result ASSIGNING <lv_result>
                           INDEX iv_current_row_index + 1.
      IF sy-subrc = 0 AND <lv_result> CP '*' && Lcl_abapgit_git_tag=>add_peel( iv_branch_name ).
        rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
      ELSE.
        rv_type = Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD normalize_branch_name.

    rv_name = iv_branch_name. " Force convert to string
    REPLACE ALL OCCURRENCES OF ` ` IN rv_name WITH '-'. " Disallow space in branch name

  ENDMETHOD.
  METHOD parse_branch_list.

    DATA: lt_result            TYPE TABLE OF string,
          lv_hash              TYPE Lif_abapgit_git_definitions=>ty_sha1,
          lv_name              TYPE string,
          lv_head_params       TYPE string,
          lv_char              TYPE c,
          lv_data              LIKE LINE OF lt_result,
          lv_current_row_index TYPE syst-tabix.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF et_list.

    CLEAR: et_list, ev_head_symref.

    lv_data = skip_first_pkt( iv_data ).
    SPLIT lv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_result.

    LOOP AT lt_result INTO lv_data.
      lv_current_row_index = sy-tabix.

      IF sy-tabix = 1 AND strlen( lv_data ) > 12 AND lv_data(4) = '0000' AND lv_data+8(3) = 'ERR'.
        lv_name = lv_data+8.
        Lcx_abapgit_exception=>raise( lv_name ).
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = Lcl_abapgit_git_utils=>get_null( ).

        SPLIT lv_name AT lv_char INTO lv_name lv_head_params.
        ev_head_symref = parse_head_params( lv_head_params ).
        IF ev_head_symref IS INITIAL AND lv_name CS 'refs/heads/'.
          ev_head_symref = lv_name.
        ENDIF.
      ELSEIF sy-tabix > 1 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 1 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        Lcx_abapgit_exception=>raise( 'No branches, create branch manually by adding file' ).
      ELSE.
        CONTINUE.
      ENDIF.

      ASSERT lv_name IS NOT INITIAL.

      APPEND INITIAL LINE TO et_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1         = lv_hash.
      <ls_branch>-name         = lv_name.
      <ls_branch>-display_name = get_display_name( lv_name ).
      <ls_branch>-type         = get_type( iv_branch_name       = lv_name
                                           it_result            = lt_result
                                           iv_current_row_index = lv_current_row_index ).
      IF <ls_branch>-name = Lif_abapgit_git_definitions=>c_head_name OR <ls_branch>-name = ev_head_symref.
        <ls_branch>-is_head = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_head_params.

    DATA: ls_match    TYPE match_result,
          ls_submatch LIKE LINE OF ls_match-submatches.

    FIND FIRST OCCURRENCE OF REGEX '\ssymref=HEAD:([^\s]+)' IN iv_data RESULTS ls_match.
    READ TABLE ls_match-submatches INTO ls_submatch INDEX 1.
    IF sy-subrc IS INITIAL.
      rv_head_symref = iv_data+ls_submatch-offset(ls_submatch-length).
    ENDIF.

  ENDMETHOD.
  METHOD skip_first_pkt.

    DATA: lv_hex    TYPE x LENGTH 1,
          lv_length TYPE i.

* channel
    ASSERT iv_data(2) = '00'.

    lv_hex = to_upper( iv_data+2(2) ).
    lv_length = lv_hex.

    rv_data = iv_data+lv_length.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_BRANCH_LIST implementation

*>>>>>>> ZCL_ABAPGIT_GIT_COMMIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_commit========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_commit========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_commit========ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWXZSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWXZSFY.





*CLASS SHRITEFUH64VYIPO5IWUYB3KWX3SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWX3SFY.



*CLASS SHRITEFUH64VYIPO5IWUYB3KWX5SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_commit DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWX5SFY.



class LCL_ABAPGIT_GIT_COMMIT implementation.
*"* method's implementations
*include methods.
  METHOD clear_missing_parents.

    "Part of #4719 to handle cut commit sequences, todo

    FIELD-SYMBOLS: <ls_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    LOOP AT ct_commits ASSIGNING <ls_commit>.

      IF is_missing( it_commits = ct_commits
                     iv_sha1  = <ls_commit>-parent1 ) = abap_true.
        CLEAR <ls_commit>-parent1.
      ENDIF.

      IF is_missing( it_commits = ct_commits
                     iv_sha1  = <ls_commit>-parent2 ) = abap_true.
        CLEAR <ls_commit>-parent2.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD extract_author_data.

    " unix time stamps are in same time zone, so ignore the zone
    FIND REGEX Lif_abapgit_definitions=>c_author_regex IN iv_author
      SUBMATCHES
      ev_author
      ev_email
      ev_time.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error author regex value='{ iv_author }'| ).
    ENDIF.

  ENDMETHOD.
  METHOD get_1st_child_commit.

    DATA: lt_1stchild_commits TYPE Lif_abapgit_git_definitions=>ty_commit_tt,
          ls_parent           LIKE LINE OF it_commit_sha1s,
          lt_commit_sha1s     LIKE it_commit_sha1s.

    FIELD-SYMBOLS: <ls_child_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    CLEAR: es_1st_commit.

* get all reachable next commits
    lt_commit_sha1s = it_commit_sha1s.
    LOOP AT ct_commits ASSIGNING <ls_child_commit> WHERE parent1 IN lt_commit_sha1s
                                                      OR parent2 IN lt_commit_sha1s.
      INSERT <ls_child_commit> INTO TABLE lt_1stchild_commits.
    ENDLOOP.

* return oldest one
    SORT lt_1stchild_commits BY time ASCENDING.
    READ TABLE lt_1stchild_commits INTO es_1st_commit INDEX 1.

* remove from available commits
    DELETE ct_commits WHERE sha1 = es_1st_commit-sha1.

* set relevant parent commit sha1s
    IF lines( lt_1stchild_commits ) = 1.
      CLEAR et_commit_sha1s.
    ELSE.
      et_commit_sha1s = it_commit_sha1s.
    ENDIF.

    ls_parent-sign   = 'I'.
    ls_parent-option = 'EQ'.
    ls_parent-low    = es_1st_commit-sha1.
    INSERT ls_parent INTO TABLE et_commit_sha1s.

  ENDMETHOD.
  METHOD get_by_branch.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          lt_objects  TYPE Lif_abapgit_definitions=>ty_objects_tt.

    li_progress = Lcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { iv_repo_url }| ).

    Lcl_abapgit_git_transport=>upload_pack_by_branch(
      EXPORTING
        iv_url          = iv_repo_url
        iv_branch_name  = iv_branch_name
        iv_deepen_level = iv_deepen_level
      IMPORTING
        ev_branch       = rs_pull_result-commit
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type <> Lif_abapgit_git_definitions=>c_type-commit.

    rs_pull_result-commits = parse_commits( lt_objects ).

    IF iv_sorted = abap_true.
      sort_commits( CHANGING ct_commits = rs_pull_result-commits ).
    ENDIF.

  ENDMETHOD.
  METHOD get_by_commit.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          lt_objects  TYPE Lif_abapgit_definitions=>ty_objects_tt.

    li_progress = Lcl_abapgit_progress=>get_instance( 1 ).

    li_progress->show(
      iv_current = 1
      iv_text    = |Get git commits { iv_repo_url }| ).

    Lcl_abapgit_git_transport=>upload_pack_by_commit(
      EXPORTING
        iv_url          = iv_repo_url
        iv_deepen_level = iv_deepen_level
        iv_hash         = iv_commit_hash
      IMPORTING
        et_objects      = lt_objects ).

    DELETE lt_objects WHERE type <> Lif_abapgit_git_definitions=>c_type-commit.

    rt_commits = parse_commits( lt_objects ).
    sort_commits( CHANGING ct_commits = rt_commits ).

  ENDMETHOD.
  METHOD is_missing.

    IF iv_sha1 IS NOT INITIAL.

      READ TABLE it_commits
        TRANSPORTING NO FIELDS
        WITH KEY sha1 = iv_sha1.
      rv_result = boolc( sy-subrc <> 0 ).

    ENDIF.

  ENDMETHOD.
  METHOD parse_commits.

    DATA: ls_commit TYPE Lif_abapgit_git_definitions=>ty_commit,
          lt_body   TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          ls_raw    TYPE Lcl_abapgit_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects,
                   <lv_body>   TYPE string.


    LOOP AT it_objects ASSIGNING <ls_object> USING KEY type
        WHERE type = Lif_abapgit_git_definitions=>c_type-commit.
      ls_raw = Lcl_abapgit_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent1 = ls_raw-parent.
      ls_commit-parent2 = ls_raw-parent2.

      SPLIT ls_raw-body AT cl_abap_char_utilities=>newline INTO TABLE lt_body.

      READ TABLE lt_body WITH KEY table_line = ' -----END PGP SIGNATURE-----' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE lt_body TO sy-tabix.
        DELETE lt_body TO 2.
      ENDIF.

      READ TABLE lt_body INDEX 1 INTO ls_commit-message.  "#EC CI_SUBRC
      " The second line is always empty. Therefore we omit it.
      LOOP AT lt_body ASSIGNING <lv_body>
                      FROM 3.
        INSERT <lv_body> INTO TABLE ls_commit-body.
      ENDLOOP.

      extract_author_data(
        EXPORTING
          iv_author = ls_raw-author
        IMPORTING
          ev_author = ls_commit-author
          ev_email  = ls_commit-email
          ev_time   = ls_commit-time ).

      APPEND ls_commit TO rt_commits.

    ENDLOOP.

  ENDMETHOD.
  METHOD reverse_sort_order.

    DATA: lt_commits           TYPE Lif_abapgit_git_definitions=>ty_commit_tt.
    FIELD-SYMBOLS: <ls_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    LOOP AT ct_commits ASSIGNING <ls_commit>.
      INSERT <ls_commit> INTO lt_commits INDEX 1.
    ENDLOOP.
    ct_commits = lt_commits.
    FREE lt_commits.

  ENDMETHOD.
  METHOD sort_commits.

    DATA: lt_sorted_commits TYPE Lif_abapgit_git_definitions=>ty_commit_tt,
          ls_next_commit    TYPE Lif_abapgit_git_definitions=>ty_commit,
          lt_parents        TYPE ty_sha1_range,
          ls_parent         LIKE LINE OF lt_parents.

    FIELD-SYMBOLS: <ls_initial_commit> TYPE Lif_abapgit_git_definitions=>ty_commit.

    " find initial commit
    READ TABLE ct_commits ASSIGNING <ls_initial_commit> WITH KEY parent1 = space.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Cannot find initial commit. Too many commits. Action not possible.| ).
    ENDIF.

    ls_parent-sign   = 'I'.
    ls_parent-option = 'EQ'.
    ls_parent-low    = <ls_initial_commit>-sha1.
    INSERT ls_parent INTO TABLE lt_parents.

    " first commit
    INSERT <ls_initial_commit> INTO TABLE lt_sorted_commits.

    " remove from available commits
    DELETE ct_commits WHERE sha1 = <ls_initial_commit>-sha1.

    DO.
      get_1st_child_commit( EXPORTING it_commit_sha1s = lt_parents
                            IMPORTING et_commit_sha1s = lt_parents
                                      es_1st_commit   = ls_next_commit
                            CHANGING  ct_commits      = ct_commits ).
      IF ls_next_commit IS INITIAL.
        EXIT. "DO
      ENDIF.
      INSERT ls_next_commit INTO TABLE lt_sorted_commits.
    ENDDO.

    ct_commits = lt_sorted_commits.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_COMMIT implementation

*>>>>>>> ZCL_ABAPGIT_GIT_TRANSPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_transport=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_transport=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_transport=====ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KWYTSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_git_transport DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWYTSFY.



class LCL_ABAPGIT_GIT_TRANSPORT implementation.
*"* method's implementations
*include methods.
  METHOD branches.

    DATA: lo_client TYPE REF TO Lcl_abapgit_http_client.


    branch_list(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
      IMPORTING
        eo_client      = lo_client
        eo_branch_list = ro_branch_list ).

    lo_client->close( ).

  ENDMETHOD.
  METHOD branch_list.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.
    CONSTANTS lc_content_type  TYPE string VALUE 'application/x-git-<service>-pack-advertisement'.

    DATA lv_data                  TYPE string.
    DATA lv_expected_content_type TYPE string.

    eo_client = Lcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = iv_service ).

    lv_expected_content_type = lc_content_type.
    REPLACE '<service>' IN lv_expected_content_type WITH iv_service.

    eo_client->check_smart_response(
        iv_expected_content_type = lv_expected_content_type
        iv_content_regex         = lc_content_regex ).

    lv_data = eo_client->get_cdata( ).

    CREATE OBJECT eo_branch_list
      EXPORTING
        iv_data = lv_data.

  ENDMETHOD.
  METHOD check_report_status.

    DATA:
      lv_string        TYPE string,
      lv_error         TYPE string,
      lv_unpack_status TYPE string,
      lv_unpack_code   TYPE string,
      lv_unpack_text   TYPE string,
      lv_commnd_status TYPE string,
      lv_commnd_code   TYPE string,
      lv_commnd_text   TYPE string.

    " Based on https://git-scm.com/docs/pack-protocol/2.2.3#_report_status
    lv_string = iv_string.

    IF lv_string = ''.
      lv_error = 'Unexpected empty reply'.
    ELSEIF strlen( lv_string ) < 4.
      lv_error = 'Missing pkt length for unpack status'.
    ELSE.
      lv_string = lv_string+4.
      SPLIT lv_string AT cl_abap_char_utilities=>newline INTO lv_unpack_status lv_string.
      SPLIT lv_unpack_status AT space INTO lv_unpack_text lv_unpack_code.

      IF lv_unpack_text <> 'unpack'.
        lv_error = 'Unexpected unpack status'.
      ELSEIF lv_unpack_code <> 'ok'.
        lv_error = |Unpack not ok ({ lv_unpack_code })|.
      ELSEIF lv_string = ''.
        lv_error = 'Unexpected command status'.
      ELSEIF strlen( lv_string ) < 4.
        lv_error = 'Missing pkt length for command status'.
      ELSE.
        lv_string = lv_string+4.
        SPLIT lv_string AT cl_abap_char_utilities=>newline INTO lv_commnd_status lv_string.
        SPLIT lv_commnd_status AT space INTO lv_commnd_code lv_commnd_text.

        IF lv_commnd_code <> 'ok'. "=ng
          " Some pre-defined error messages
          IF lv_commnd_text CP '*pre-receive hook declined*'.
            lv_error = 'Pre-receive hook declined'.
          ELSEIF lv_commnd_text CP '*protected branch hook declined*'.
            lv_error = 'Protected branch hook declined'.
          ELSEIF lv_commnd_text CP '*push declined due to email privacy*'.
            lv_error = 'Push declined due to email privacy'.
          ELSEIF lv_commnd_text CP '*funny refname*'.
            lv_error = 'Funny refname'.
          ELSEIF lv_commnd_text CP '*failed to update ref*'.
            lv_error = 'Failed to update ref'.
          ELSEIF lv_commnd_text CP '*missing necessary objects*'.
            lv_error = 'Missing necessary objects'.
          ELSEIF lv_commnd_text CP '*refusing to delete the current branch*'.
            lv_error = 'Branch delete not allowed'.
          ELSEIF lv_commnd_text CP '*cannot lock ref*reference already exists*'.
            lv_error = 'Branch already exists'.
          ELSEIF lv_commnd_text CP '*cannot lock ref*but expected*'.
            lv_error = 'Branch cannot be locked'.
          ELSEIF lv_commnd_text CP '*invalid committer*'.
            lv_error = 'Invalid committer'.
          ELSE.
            " Otherwise return full error message
            lv_error = lv_commnd_text.
          ENDIF.
        ELSEIF strlen( lv_string ) < 4.
          lv_error = 'Missing flush-pkt'.
        ELSEIF lv_string <> '0000' AND lv_string <> '00000000'.
          " We update only one reference at a time so this should be the end
          lv_error = 'Unexpected end of status (flush-pkt)'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_error IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( |Git protocol error: { lv_error }| ).
    ENDIF.

  ENDMETHOD.
  METHOD find_branch.

    branch_list(
      EXPORTING
        iv_url          = iv_url
        iv_service      = iv_service
      IMPORTING
        eo_client       = eo_client
        eo_branch_list  = eo_branch_list ).

    IF ev_branch IS SUPPLIED.
      ev_branch = eo_branch_list->find_by_name( iv_branch_name )-sha1.
    ENDIF.

  ENDMETHOD.
  METHOD parse.

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = Lcl_abapgit_git_utils=>length_utf8_hex( cv_data ).

      IF lv_len > xstrlen( cv_data ).
        Lcx_abapgit_exception=>raise( 'parse, string length too large' ).
      ENDIF.

      lv_contents = cv_data(lv_len).
      IF lv_len = 0.
        cv_data = cv_data+4.
        CONTINUE.
      ELSE.
        cv_data = cv_data+lv_len.
      ENDIF.

      lv_contents = lv_contents+4.

      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = lc_band1.
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.
  METHOD receive_pack.

    DATA: lo_client   TYPE REF TO Lcl_abapgit_http_client,
          lv_cmd_pkt  TYPE string,
          lv_line     TYPE string,
          lv_tmp      TYPE xstring,
          lv_xstring  TYPE xstring,
          lv_string   TYPE string,
          lv_cap_list TYPE string,
          lv_buffer   TYPE string.


    find_branch(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-receive
        iv_branch_name = iv_branch_name
      IMPORTING
        eo_client      = lo_client ).

    lo_client->set_headers(
      iv_url     = iv_url
      iv_service = c_service-receive ).

    lv_cap_list = 'report-status'.

    lv_line = iv_old &&
              ` ` &&
              iv_new &&
              ` ` &&
              iv_branch_name &&
              Lcl_abapgit_git_utils=>get_null( ) &&
              ` ` &&
              lv_cap_list &&
              cl_abap_char_utilities=>newline.
    lv_cmd_pkt = Lcl_abapgit_git_utils=>pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    lv_xstring = lo_client->send_receive_close( lv_xstring ).

    lv_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ).

    check_report_status( lv_string ).

  ENDMETHOD.
  METHOD upload_pack.

    DATA: lv_capa    TYPE string,
          lv_line    TYPE string,
          lv_buffer  TYPE string,
          lv_xstring TYPE xstring,
          lv_pack    TYPE xstring.

    FIELD-SYMBOLS: <lv_hash> LIKE LINE OF it_hashes.


    io_client->set_headers( iv_url     = iv_url
                            iv_service = c_service-upload ).

    LOOP AT it_hashes FROM 1 ASSIGNING <lv_hash>.
      IF sy-tabix = 1.
        lv_capa = 'side-band-64k no-progress multi_ack'.
        lv_line = 'want' && ` ` && <lv_hash>
          && ` ` && lv_capa && cl_abap_char_utilities=>newline.
      ELSE.
        lv_line = 'want' && ` ` && <lv_hash>
          && cl_abap_char_utilities=>newline.
      ENDIF.
      lv_buffer = lv_buffer && Lcl_abapgit_git_utils=>pkt_string( lv_line ).
    ENDLOOP.

    IF iv_deepen_level > 0.
      lv_buffer = lv_buffer && Lcl_abapgit_git_utils=>pkt_string( |deepen { iv_deepen_level }| &&
        cl_abap_char_utilities=>newline ).
    ENDIF.

    lv_buffer = lv_buffer
             && '0000'
             && '0009done' && cl_abap_char_utilities=>newline.

    lv_xstring = io_client->send_receive_close( Lcl_abapgit_convert=>string_to_xstring_utf8( lv_buffer ) ).

    parse( IMPORTING ev_pack = lv_pack
           CHANGING  cv_data = lv_xstring ).

    IF lv_pack IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Response could not be parsed - empty pack returned.' ).
    ENDIF.

    rt_objects = Lcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.
  METHOD upload_pack_by_branch.

    DATA: lo_client TYPE REF TO Lcl_abapgit_http_client,
          lt_hashes TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF it_branches.


    CLEAR: et_objects,
           ev_branch.

    find_branch(
      EXPORTING
        iv_url         = iv_url
        iv_service     = c_service-upload
        iv_branch_name = iv_branch_name
      IMPORTING
        eo_client      = lo_client
        ev_branch      = ev_branch ).

    IF it_branches IS INITIAL.
      APPEND ev_branch TO lt_hashes.
    ELSE.
      LOOP AT it_branches ASSIGNING <ls_branch>.
        APPEND <ls_branch>-sha1 TO lt_hashes.
      ENDLOOP.
    ENDIF.

    et_objects = upload_pack( io_client       = lo_client
                              iv_url          = iv_url
                              iv_deepen_level = iv_deepen_level
                              it_hashes       = lt_hashes ).

  ENDMETHOD.
  METHOD upload_pack_by_commit.

    DATA: lo_client TYPE REF TO Lcl_abapgit_http_client,
          lt_hashes TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.


    CLEAR: et_objects,
           ev_commit.

    APPEND iv_hash TO lt_hashes.
    ev_commit = iv_hash.

    lo_client = Lcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = c_service-upload ).

    et_objects = upload_pack( io_client       = lo_client
                              iv_url          = iv_url
                              iv_deepen_level = iv_deepen_level
                              it_hashes       = lt_hashes ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_TRANSPORT implementation

*>>>>>>> ZCL_ABAPGIT_GUI_BUTTONS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_buttons=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_buttons=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_BUTTONS implementation.
*"* method's implementations
*include methods.
  METHOD advanced.
    rv_html_string = Lcl_abapgit_html=>icon(
      iv_name = 'tools-solid'
      iv_hint = 'Utilities' ).
  ENDMETHOD.
  METHOD experimental.
    rv_html_string = Lcl_abapgit_html=>icon(
      iv_name = 'vial-solid/red'
      iv_hint = 'Experimental Features are Enabled' ).
  ENDMETHOD.
  METHOD help.
    rv_html_string = Lcl_abapgit_html=>icon(
      iv_name = 'question-circle-solid'
      iv_hint = 'Help' ).
  ENDMETHOD.
  METHOD new_offline.
    rv_html_string = Lcl_abapgit_html=>icon( 'plug' ) && ' New Offline'.
  ENDMETHOD.
  METHOD new_online.
    rv_html_string = Lcl_abapgit_html=>icon( 'cloud-upload-alt' ) && ' New Online'.
  ENDMETHOD.
  METHOD repo_list.
    rv_html_string = Lcl_abapgit_html=>icon( 'bars' ) && ' Repository List'.
  ENDMETHOD.
  METHOD settings.
    rv_html_string = Lcl_abapgit_html=>icon( 'cog' ) && ' Settings'.
  ENDMETHOD.
  METHOD flow.
    rv_html_string = Lcl_abapgit_html=>icon( 'flow' ) && ' Flow'.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_BUTTONS implementation

*>>>>>>> ZCL_ABAPGIT_GUI_EVENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_event=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_event=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_event=========ccau.



*CLASS zcl_abapgit_gui_event DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWZKSFY.


class LCL_ABAPGIT_GUI_EVENT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    " Edge Webview control returns upper case action but abapGit requires lower case (#4841)
    Lif_abapgit_gui_event~mi_gui_services = ii_gui_services.
    Lif_abapgit_gui_event~mv_action       = to_lower( iv_action ).
    Lif_abapgit_gui_event~mv_getdata      = iv_getdata.
    Lif_abapgit_gui_event~mt_postdata     = it_postdata.

    IF ii_gui_services IS BOUND.
      Lif_abapgit_gui_event~mv_current_page_name = ii_gui_services->get_current_page_name( ).
    ENDIF.

  ENDMETHOD.
  METHOD fields_to_map.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF it_fields.

    CREATE OBJECT ro_string_map EXPORTING iv_case_insensitive = abap_true.
    LOOP AT it_fields ASSIGNING <ls_field>.
      ro_string_map->set(
        iv_key = <ls_field>-name
        iv_val = <ls_field>-value ).
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event~form_data.

    IF mo_form_data IS NOT BOUND.
      mo_form_data = fields_to_map( parse_post_form_data( Lif_abapgit_gui_event~mt_postdata ) ).
      mo_form_data->freeze( ).
    ENDIF.
    ro_string_map = mo_form_data.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event~query.

    IF mo_query IS NOT BOUND.
      mo_query = fields_to_map( parse_fields( Lif_abapgit_gui_event~mv_getdata ) ).
      mo_query->freeze( ).
    ENDIF.
    ro_string_map = mo_query.

  ENDMETHOD.
  METHOD class_constructor.

    CONSTANTS lc_nbsp TYPE xstring VALUE 'C2A0'. " &nbsp;

    TRY.
        gv_non_breaking_space = Lcl_abapgit_convert=>xstring_to_string_utf8( lc_nbsp ).
      CATCH Lcx_abapgit_exception.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.
  METHOD field_keys_to_upper.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF ct_fields.

    LOOP AT ct_fields ASSIGNING <ls_field>.
      <ls_field>-name = to_upper( <ls_field>-name ).
    ENDLOOP.

  ENDMETHOD.
  METHOD parse_fields.

    DATA:
      lt_substrings TYPE string_table,
      ls_field      LIKE LINE OF rt_fields.

    FIELD-SYMBOLS <lv_substring> LIKE LINE OF lt_substrings.

    SPLIT iv_string AT '&' INTO TABLE lt_substrings.

    LOOP AT lt_substrings ASSIGNING <lv_substring>.

      CLEAR ls_field.
      " On attempt to change unescaping -> run unit tests to check !

      " Unescape name and value separately
      ls_field-name = unescape( substring_before(
        val = <lv_substring>
        sub = '=' ) ).

      ls_field-value = unescape( substring_after(
        val = <lv_substring>
        sub = '=' ) ).

      IF ls_field IS INITIAL. " Not a field with proper structure
        CONTINUE.
      ENDIF.

      APPEND ls_field TO rt_fields.

    ENDLOOP.

    IF iv_upper_cased = abap_true.
      field_keys_to_upper( CHANGING ct_fields = rt_fields ).
    ENDIF.

  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_instance
      EXPORTING
        ii_gui_services = ii_gui_services
        iv_action       = iv_action
        iv_getdata      = iv_getdata
        it_postdata     = it_postdata.
  ENDMETHOD.
  METHOD parse_fields_upper_case_name.

    rt_fields = parse_fields(
      iv_string      = iv_string
      iv_upper_cased = abap_true ).

  ENDMETHOD.
  METHOD parse_post_form_data.

    DATA lv_serialized_post_data TYPE string.

    lv_serialized_post_data = translate_postdata( it_post_data ).
    IF iv_upper_cased = abap_true.
      rt_fields = parse_fields_upper_case_name( lv_serialized_post_data ).
    ELSE.
      rt_fields = parse_fields( lv_serialized_post_data ).
    ENDIF.

  ENDMETHOD.
  METHOD translate_postdata.

    DATA: lt_post_data       TYPE Lif_abapgit_html_viewer=>ty_post_data,
          ls_last_line       LIKE LINE OF it_postdata,
          lv_last_line_index TYPE i.

    IF it_postdata IS INITIAL.
      RETURN. "Nothing to do
    ENDIF.

    lt_post_data = it_postdata.

    "Save the last line for separate merge, because we don't need its trailing spaces
    WHILE ls_last_line IS INITIAL.
      lv_last_line_index = lines( lt_post_data ).
      READ TABLE lt_post_data INTO ls_last_line INDEX lv_last_line_index.
      DELETE lt_post_data INDEX lv_last_line_index.
    ENDWHILE.

    CONCATENATE LINES OF lt_post_data INTO rv_string
      IN CHARACTER MODE RESPECTING BLANKS.
    CONCATENATE rv_string ls_last_line INTO rv_string
      IN CHARACTER MODE.

  ENDMETHOD.
  METHOD unescape.

* do not use cl_http_utility as it does strange things with the encoding
    rv_string = iv_string.

* todo, more to be added here
    REPLACE ALL OCCURRENCES OF '%3A' IN rv_string WITH ':' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%3F' IN rv_string WITH '?' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%3D' IN rv_string WITH '=' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%2F' IN rv_string WITH '/' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%25' IN rv_string WITH '%' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF '%26' IN rv_string WITH '&' IGNORING CASE.
    REPLACE ALL OCCURRENCES OF gv_non_breaking_space IN rv_string WITH ` `.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_EVENT implementation

*>>>>>>> ZCL_ABAPGIT_GUI_HOTKEY_CTL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_hotkey_ctl====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_hotkey_ctl====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_HOTKEY_CTL implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    ms_user_settings = Lcl_abapgit_persistence_user=>get_instance( )->get_settings( ).

  ENDMETHOD.
  METHOD render_scripts.

    DATA lv_json TYPE string.

    FIELD-SYMBOLS: <ls_hotkey> LIKE LINE OF it_hotkeys.

    lv_json = `{`.

    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <ls_hotkey>-hotkey }" : "{ <ls_hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( |setKeyBindings({ lv_json });| ).

  ENDMETHOD.
  METHOD should_show_hint.
    IF gv_hint_was_shown = abap_false.
      rv_yes = abap_true.
      gv_hint_was_shown = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey LIKE LINE OF rt_hotkey_actions.

    ls_hotkey-ui_component = 'Hotkeys'.
    ls_hotkey-action       = c_showhotkeys_action.
    ls_hotkey-description  = 'Show Hotkeys Help'.
    ls_hotkey-hotkey       = '?'.
    INSERT ls_hotkey INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~get_registered_hotkeys.
    rt_registered_hotkeys = mt_hotkeys.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~register_hotkeys.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF it_hotkeys.

    " Compress duplicates
    LOOP AT it_hotkeys ASSIGNING <ls_hotkey>.
      READ TABLE mt_hotkeys WITH KEY hotkey = <ls_hotkey>-hotkey TRANSPORTING NO FIELDS.
      IF sy-subrc = 0. " If found command with same hotkey
        DELETE mt_hotkeys INDEX sy-tabix. " Later registered commands enjoys the priority
      ENDIF.

      IF ms_user_settings-link_hints_enabled = abap_true AND
         ms_user_settings-link_hint_key      = <ls_hotkey>-hotkey.
        " Link hint activation key is more important
        CONTINUE.
      ENDIF.

      APPEND <ls_hotkey> TO mt_hotkeys.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~reset.
    CLEAR mt_hotkeys.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkey_ctl~set_visible.

    mv_visible = iv_visible.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA:
      lv_hint               TYPE string,
      lt_registered_hotkeys TYPE Lif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr,
      lv_hotkey             TYPE string,
      ls_user_settings      TYPE Lif_abapgit_definitions=>ty_s_user_settings.

    FIELD-SYMBOLS <ls_hotkey> LIKE LINE OF lt_registered_hotkeys.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lt_registered_hotkeys = Lif_abapgit_gui_hotkey_ctl~get_registered_hotkeys( ).
    SORT lt_registered_hotkeys BY ui_component description.

    register_deferred_script( render_scripts( lt_registered_hotkeys ) ).

    " Render hotkeys
    ri_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_registered_hotkeys ASSIGNING <ls_hotkey>.
      ri_html->add( |<li>|
        && |<span class="key-id">{ <ls_hotkey>-hotkey }</span>|
        && |<span class="key-descr">{ <ls_hotkey>-description }</span>|
        && |</li>| ).
    ENDLOOP.

    " render link hints activation key
    ls_user_settings = Lcl_abapgit_persistence_user=>get_instance( )->get_settings( ).
    IF ls_user_settings-link_hints_enabled = abap_true.
      ri_html->add( |<li>|
         && |<span class="key-id">{ ls_user_settings-link_hint_key }</span>|
         && |<span class="key-descr">Link Hints</span>|
         && |</li>| ).
      ri_html->add( |<li>|
         && |<span class="key-id">y{ ls_user_settings-link_hint_key }</span>|
         && |<span class="key-descr">Copy Link Text</span>|
         && |</li>| ).
    ENDIF.

    ri_html->add( '</ul>' ).

    CLEAR lv_hotkey.

    READ TABLE lt_registered_hotkeys ASSIGNING <ls_hotkey>
      WITH KEY action = c_showhotkeys_action.
    IF sy-subrc = 0.
      lv_hotkey = <ls_hotkey>-hotkey.
    ENDIF.

    lv_hint = |Close window with upper right corner 'X'|.
    IF lv_hotkey IS NOT INITIAL.
      lv_hint = lv_hint && | or press '{ <ls_hotkey>-hotkey }'|.
    ENDIF.

    ri_html = Lcl_abapgit_gui_chunk_lib=>render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = lv_hint
      iv_hide       = boolc( mv_visible = abap_false )
      iv_scrollable = abap_false
      io_content    = ri_html ).

    IF lv_hotkey IS NOT INITIAL AND should_show_hint( ) = abap_true.
      ri_html->add( |<div id="hotkeys-hint" class="corner-hint">|
        && |Press '{ <ls_hotkey>-hotkey }' to get keyboard shortcuts list|
        && |</div>| ).
    ENDIF.

    " Always reset visibility here. Closing of the popup has to be done by the
    " user and is handeled in JS.
    mv_visible = abap_false.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_HOTKEY_CTL implementation

*>>>>>>> ZCL_ABAPGIT_GUI_HTML_PROCESSOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_html_processorccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_html_processorccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_html_processorccau.



*CLASS SHRITEFUH64VYIPO5IWUYB3KWZNSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_gui_html_processor DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWZNSFY.



class LCL_ABAPGIT_GUI_HTML_PROCESSOR implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mi_asset_man = ii_asset_man.
  ENDMETHOD.
  METHOD find_head_offset.

    rv_head_end = find( val = iv_html
                        regex = |{ cl_abap_char_utilities=>newline }?\\s*</head>|
                        case = abap_false ).
    IF rv_head_end <= 0.
      rv_head_end = find( val = iv_html
                          regex = |</head>|
                          case = abap_false ).
      IF rv_head_end <= 0.
        Lcx_abapgit_exception=>raise( 'HTML preprocessor: </head> not found' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD is_preserved.
    READ TABLE mt_preserve_css TRANSPORTING NO FIELDS WITH KEY table_line = iv_css_url.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD patch_html.

    CONSTANTS lc_css_re TYPE string VALUE `<link\s+rel="stylesheet"\s+type="text/css"\s+href="(\S+)">`.

    DATA lv_head_end TYPE i.
    DATA lo_css_re   TYPE REF TO cl_abap_regex.
    DATA lo_matcher  TYPE REF TO cl_abap_matcher.
    DATA lv_css_path TYPE string.
    DATA lv_marker   TYPE string.

    DATA lv_off TYPE i.
    DATA lv_len TYPE i.
    DATA lv_cur TYPE i.

    DATA lv_css_build TYPE string VALUE '<link rel="stylesheet" type="text/css" href="$BUILD_NAME">'.
    REPLACE FIRST OCCURRENCE OF '$BUILD_NAME' IN lv_css_build WITH c_css_build_name. " Mmmm

    CLEAR: ev_html, et_css_urls.

    lv_head_end = find_head_offset( iv_html ).

    CREATE OBJECT lo_css_re
      EXPORTING
        ignore_case = abap_true
        pattern     = lc_css_re.

    lo_matcher = lo_css_re->create_matcher( text = substring( val = iv_html len = lv_head_end ) ).
    WHILE lo_matcher->find_next( ) = abap_true.
      lv_css_path = lo_matcher->get_submatch( 1 ).
      IF abap_false = is_preserved( lv_css_path ).
        lv_off = lo_matcher->get_offset( ).
        lv_len = lo_matcher->get_length( ).
        ev_html = ev_html && substring( val = iv_html
                                        off = lv_cur
                                        len = lv_off - lv_cur ).
        ev_html = ev_html && c_comment_start && substring( val = iv_html
                                                           off = lv_off
                                                           len = lv_len ) && c_comment_end.
        lv_cur  = lv_off + lv_len.
        APPEND lv_css_path TO et_css_urls.
      ENDIF.
    ENDWHILE.

    ev_html = ev_html && substring( val = iv_html
                                    off = lv_cur
                                    len = lv_head_end - lv_cur ).
    IF lines( et_css_urls ) > 0.
      lv_marker = cl_abap_char_utilities=>newline
        && `    ` " Assume 4 space indent, maybe improve and detect ?
        && c_preprocess_marker
        && cl_abap_char_utilities=>newline
        && `    `.
      ev_html = ev_html && lv_marker && lv_css_build.
    ENDIF.
    ev_html = ev_html && substring( val = iv_html
                                    off = lv_head_end ).

  ENDMETHOD.
  METHOD preserve_css.
    APPEND iv_css_url TO mt_preserve_css.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_html_processor~process.

    DATA lo_css_processor TYPE REF TO Lcl_abapgit_gui_css_processor.
    DATA lt_css_urls TYPE string_table.
    DATA lv_css_build TYPE string.

    FIELD-SYMBOLS <lv_url> LIKE LINE OF lt_css_urls.

    patch_html(
      EXPORTING
        iv_html = iv_html
      IMPORTING
        ev_html = rv_html
        et_css_urls = lt_css_urls ).

    IF lines( lt_css_urls ) > 0.
      CREATE OBJECT lo_css_processor
        EXPORTING
          ii_asset_manager = mi_asset_man.

      LOOP AT lt_css_urls ASSIGNING <lv_url>.
        lo_css_processor->add_file( <lv_url> ).
      ENDLOOP.

      lv_css_build = lo_css_processor->process( ).

      ii_gui_services->cache_asset(
        iv_url     = |{ c_css_build_name }|
        iv_type    = 'text'
        iv_subtype = 'css'
        iv_text    = lv_css_build ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_HTML_PROCESSOR implementation

*>>>>>>> ZCL_ABAPGIT_GUI_MENUS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_menus=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_menus=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_MENUS implementation.
*"* method's implementations
*include methods.
  METHOD advanced.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-advanced'.

    ro_menu->add(
      iv_txt = 'Database Utility'
      iv_act = Lif_abapgit_definitions=>c_action-go_db
    )->add(
      iv_txt = 'Package to ZIP'
      iv_act = Lif_abapgit_definitions=>c_action-zip_package
    )->add(
      iv_txt = 'Transport to ZIP'
      iv_act = Lif_abapgit_definitions=>c_action-zip_transport
    )->add(
      iv_txt = 'Object to Files'
      iv_act = Lif_abapgit_definitions=>c_action-zip_object
    )->add(
      iv_txt = 'Debug Info'
      iv_act = Lif_abapgit_definitions=>c_action-go_debuginfo ).

    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_windows( ) = abap_true.
      ro_menu->add(
        iv_txt = 'Open IE DevTools'
        iv_act = Lif_abapgit_definitions=>c_action-ie_devtools ).
    ENDIF.

  ENDMETHOD.
  METHOD back.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-back'.

    ro_menu->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD experimental.

    IF Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ) IS NOT INITIAL.
      io_menu->add(
        iv_txt = Lcl_abapgit_gui_buttons=>experimental( )
        iv_act = Lif_abapgit_definitions=>c_action-go_settings ).
    ENDIF.

  ENDMETHOD.
  METHOD help.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-help'.

    ro_menu->add(
      iv_txt = 'Tutorial'
      iv_act = Lif_abapgit_definitions=>c_action-go_tutorial
    )->add(
      iv_txt = 'Documentation'
      iv_act = Lif_abapgit_definitions=>c_action-documentation
    )->add(
      iv_txt = 'Explore'
      iv_act = Lif_abapgit_definitions=>c_action-go_explore
    )->add(
      iv_txt = 'Changelog'
      iv_act = Lif_abapgit_definitions=>c_action-changelog
    )->add(
      iv_txt = 'Hotkeys'
      iv_act = Lif_abapgit_definitions=>c_action-show_hotkeys ).

  ENDMETHOD.
  METHOD repo_settings.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-repo-settings'.

    ro_menu->add(
      iv_txt = 'Repository'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_settings )
    )->add(
      iv_txt = 'Local'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_local_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_local_settings )
    )->add(
      iv_txt = 'Remote'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_remote_settings )
    )->add(
      iv_txt = 'Background'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_background }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_background )
    )->add(
      iv_txt = 'Stats'
      iv_act = |{ Lif_abapgit_definitions=>c_action-repo_infos }?key={ iv_key }|
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-repo_infos ) ).

    Lcl_abapgit_exit=>get_instance( )->enhance_repo_toolbar(
      io_menu = ro_menu
      iv_key  = iv_key
      iv_act  = iv_act ).

  ENDMETHOD.
  METHOD settings.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-settings'.

    ro_menu->add(
      iv_txt = 'Global'
      iv_act = Lif_abapgit_definitions=>c_action-go_settings
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-go_settings )
    )->add(
      iv_txt = 'Personal'
      iv_act = Lif_abapgit_definitions=>c_action-go_settings_personal
      iv_cur = boolc( iv_act = Lif_abapgit_definitions=>c_action-go_settings_personal ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_MENUS implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_RUN_BCKG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_run_bckg=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_run_bckg=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_RUN_BCKG implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_run_bckg.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Background Run'
      io_page_menu       = Lcl_abapgit_gui_menus=>back( )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD run.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception,
          lv_text  TYPE string,
          lv_line  TYPE i VALUE 1.


    TRY.
        Lcl_abapgit_background=>run( ).

        DO.
          READ LINE lv_line LINE VALUE INTO lv_text.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND lv_text TO mt_text.
          lv_line = lv_line + 1.
        ENDDO.
      CATCH Lcx_abapgit_exception INTO lx_error.
        APPEND lx_error->get_text( ) TO mt_text.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.
    rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA: lv_text LIKE LINE OF mt_text.

    register_handlers( ).

    run( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ri_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_RUN_BCKG implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_BCKG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_bckgccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_bckgccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_BCKG implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_bckg.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Background Mode'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_background )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    DATA:
      lt_methods TYPE Lcl_abapgit_background=>ty_methods,
      ls_method  LIKE LINE OF lt_methods,
      lv_hint    TYPE string.

    lt_methods = Lcl_abapgit_background=>list_methods( ).

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-background-form'
                iv_help_page = 'https://docs.abapgit.org/settings-background-mode.html' ).

    ro_form->start_group(
      iv_name          = c_id-mode_selection
      iv_label         = 'Mode'
    )->radio(
      iv_name          = c_id-method
      iv_default_value = ''
      iv_label         = 'Selection'
      iv_hint          = 'Define the action that will be executed in background mode'
    )->option(
      iv_label         = 'Do Nothing'
      iv_value         = '' ).

    LOOP AT lt_methods INTO ls_method.
      ro_form->option(
        iv_label       = ls_method-description
        iv_value       = ls_method-class ).
    ENDLOOP.

    ro_form->table(
      iv_name        = c_id-settings
      iv_hint        = 'Settings required for selected background action'
      iv_label       = 'Additional Settings'
    )->column(
      iv_label       = 'Key'
      iv_width       = '50%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Value'
      iv_width       = '50%' ).

    lv_hint = 'Password will be saved in clear text!'.

    ro_form->start_group(
      iv_name        = c_id-authentication
      iv_label       = 'HTTP Authentication (Optional)'
      iv_hint        = lv_hint
    )->text(
      iv_name        = c_id-username
      iv_label       = 'Username'
      iv_hint        = lv_hint
    )->text(
      iv_name        = c_id-password
      iv_label       = 'Password'
      iv_hint        = lv_hint
      iv_placeholder = lv_hint ).

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Run Background Logic'
      iv_action      = Lif_abapgit_definitions=>c_action-go_background_run
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_persist.

    DATA lo_per TYPE REF TO Lcl_abapgit_persist_background.

    CREATE OBJECT lo_per.

    TRY.
        rs_persist = lo_per->get_by_key( mo_repo->get_key( ) ).
      CATCH Lcx_abapgit_not_found.
        CLEAR rs_persist.
    ENDTRY.

  ENDMETHOD.
  METHOD read_settings.

    DATA:
      ls_per      TYPE Lcl_abapgit_persist_background=>ty_background,
      lv_row      TYPE i,
      lv_val      TYPE string,
      lt_settings LIKE ls_per-settings,
      ls_settings LIKE LINE OF ls_per-settings.

    ls_per = read_persist( ).
    CREATE OBJECT ro_form_data.

    " Mode Selection
    ro_form_data->set(
      iv_key = c_id-method
      iv_val = ls_per-method ).

    " Mode Settings
    IF ls_per-method IS NOT INITIAL.

      lt_settings = ls_per-settings.

      " skip invalid values, from old background logic
      IF ls_per-method <> 'push' AND ls_per-method <> 'pull' AND ls_per-method <> 'nothing'.
        CALL METHOD (ls_per-method)=>Lif_abapgit_background~get_settings
          CHANGING
            ct_settings = lt_settings.
      ENDIF.

      LOOP AT lt_settings INTO ls_settings.
        lv_row = lv_row + 1.
        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              lv_val = ls_settings-key.
            WHEN 2.
              lv_val = ls_settings-value.
          ENDCASE.
          ro_form_data->set(
            iv_key = |{ c_id-settings }-{ lv_row }-{ sy-index }|
            iv_val = lv_val ).
        ENDDO.
      ENDLOOP.

    ENDIF.

    mv_settings_count = lv_row.

    ro_form_data->set(
      iv_key = |{ c_id-settings }-{ Lif_abapgit_html_form=>c_rows }|
      iv_val = |{ mv_settings_count }| ).

    " Authentication
    ro_form_data->set(
      iv_key = c_id-username
      iv_val = ls_per-username ).
    ro_form_data->set(
      iv_key = c_id-password
      iv_val = ls_per-password ).

  ENDMETHOD.
  METHOD save_settings.

    DATA:
      lo_persistence TYPE REF TO Lcl_abapgit_persist_background,
      ls_per         TYPE Lcl_abapgit_persist_background=>ty_background,
      lt_settings    LIKE ls_per-settings.

    FIELD-SYMBOLS:
      <ls_settings> LIKE LINE OF ls_per-settings.

    ls_per-key = mo_repo->get_key( ).

    " Mode Selection
    ls_per-method = mo_form_data->get( c_id-method ).

    " Mode Settings
    IF ls_per-method IS NOT INITIAL.

      lt_settings = ls_per-settings.

      " skip invalid values, from old background logic
      IF ls_per-method <> 'push' AND ls_per-method <> 'pull' AND ls_per-method <> 'nothing'.
        CALL METHOD (ls_per-method)=>Lif_abapgit_background~get_settings
          CHANGING
            ct_settings = lt_settings.
      ENDIF.

      LOOP AT lt_settings ASSIGNING <ls_settings>.
        <ls_settings>-value = mo_form_data->get( |{ c_id-settings }-{ sy-tabix }-2| ).
      ENDLOOP.

      ls_per-settings = lt_settings.

    ENDIF.

    " Authentication
    ls_per-username = mo_form_data->get( c_id-username ).
    ls_per-password = mo_form_data->get( c_id-password ).

    CREATE OBJECT lo_persistence.

    IF ls_per-method IS INITIAL.
      lo_persistence->delete( ls_per-key ).
    ELSE.
      lo_persistence->modify( ls_per ).
    ENDIF.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-save.
        save_settings( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render(
      iv_form_class = 'w800px'
      io_values     = mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_BCKG implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_GLOB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_globccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_globccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_GLOB implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_glob.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Global Settings'
      io_page_menu       = Lcl_abapgit_gui_menus=>settings( Lif_abapgit_definitions=>c_action-go_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
      iv_form_id   = 'global-setting-form'
      iv_help_page = 'https://docs.abapgit.org/guide-settings-global.html' ).

    ro_form->start_group(
      iv_name        = c_id-proxy_settings
      iv_label       = 'Proxy Settings'
    )->text(
      iv_name        = c_id-proxy_url
      iv_label       = 'Proxy Host'
      iv_hint        = 'Hostname or IP of proxy required to access the Internet (do not enter http://)'
      iv_placeholder = 'Hostname or IP without http://'
    )->number(
      iv_name        = c_id-proxy_port
      iv_label       = 'Proxy Port'
      iv_hint        = 'Port of proxy required to access the Internet'
      iv_min         = 0
      iv_max         = 65535
    )->checkbox(
      iv_name        = c_id-proxy_auth
      iv_label       = 'Proxy Authentication'
      iv_hint        = 'Check, if proxy requires you to login'
    )->textarea(
      iv_name        = c_id-proxy_bypass
      iv_label       = 'Proxy Bypass'
      iv_hint        = 'List of hosts/domains for which to bypass using proxy'
    )->start_group(
      iv_name        = c_id-commit_settings
      iv_label       = 'Commit Message Settings'
    )->number(
      iv_name        = c_id-commitmsg_comment_length
      iv_required    = abap_true
      iv_label       = 'Maximum Length of Comment'
      iv_hint        = |At least { Lcl_abapgit_settings=>c_commitmsg_comment_length_dft } characters|
      iv_min         = Lcl_abapgit_settings=>c_commitmsg_comment_length_dft
    )->text(
      iv_name        = c_id-commitmsg_comment_deflt
      iv_label       = 'Default Text For Comment'
      iv_hint        = 'You can use $OBJECT or $FILE to include the number of objects/files'
    )->number(
      iv_name        = c_id-commitmsg_body_size
      iv_required    = abap_true
      iv_label       = 'Maximum Line Size of Body'
      iv_hint        = |At least { Lcl_abapgit_settings=>c_commitmsg_body_size_dft } characters|
      iv_min         = Lcl_abapgit_settings=>c_commitmsg_body_size_dft
    )->checkbox(
      iv_name        = c_id-commitmsg_hide_author
      iv_label       = 'Hide Author Fields' ).

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      ro_form->start_group(
        iv_name        = c_id-devint_settings
        iv_label       = 'Development Internal Settings'
      )->checkbox(
        iv_name        = c_id-run_critical_tests
        iv_label       = 'Enable Critical Unit Tests'
      )->text(
        iv_name        = c_id-experimental_features
        iv_label       = 'Experimental Features'
        iv_hint        = 'Set to "X" to enable all features or add feature values as a comma-separated list' ).
    ENDIF.

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_proxy_bypass.

    DATA:
      lt_proxy_bypass TYPE Lif_abapgit_definitions=>ty_range_proxy_bypass_url,
      ls_proxy_bypass LIKE LINE OF lt_proxy_bypass,
      lv_val          TYPE string.

    lt_proxy_bypass = io_settings->get_proxy_bypass( ).
    LOOP AT lt_proxy_bypass INTO ls_proxy_bypass.
      lv_val = lv_val && ls_proxy_bypass-low && cl_abap_char_utilities=>newline.
    ENDLOOP.

    io_form_data->set(
      iv_key = c_id-proxy_bypass
      iv_val = lv_val ).

  ENDMETHOD.
  METHOD read_settings.

    " Get settings from DB
    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).
    CREATE OBJECT ro_form_data.

    " Proxy
    ro_form_data->set(
      iv_key = c_id-proxy_url
      iv_val = mo_settings->get_proxy_url( ) ).
    ro_form_data->set(
      iv_key = c_id-proxy_port
      iv_val = mo_settings->get_proxy_port( ) ).
    ro_form_data->set(
      iv_key = c_id-proxy_auth
      iv_val = boolc( mo_settings->get_proxy_authentication( ) = abap_true ) ) ##TYPE.

    read_proxy_bypass(
      io_settings = mo_settings
      io_form_data = ro_form_data ).

    " Commit Message
    ro_form_data->set(
      iv_key = c_id-commitmsg_comment_length
      iv_val = |{ mo_settings->get_commitmsg_comment_length( ) }| ).
    ro_form_data->set(
      iv_key = c_id-commitmsg_comment_deflt
      iv_val = mo_settings->get_commitmsg_comment_default( ) ).
    ro_form_data->set(
      iv_key = c_id-commitmsg_body_size
      iv_val = |{ mo_settings->get_commitmsg_body_size( ) }| ).
    ro_form_data->set(
      iv_key = c_id-commitmsg_hide_author
      iv_val = boolc( mo_settings->get_commitmsg_hide_author( ) = abap_true ) ) ##TYPE.

    " Dev Internal
    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      ro_form_data->set(
        iv_key = c_id-run_critical_tests
        iv_val = boolc( mo_settings->get_run_critical_tests( ) = abap_true ) ) ##TYPE.
      ro_form_data->set(
        iv_key = c_id-experimental_features
        iv_val = mo_settings->get_experimental_features( ) ).
    ENDIF.

  ENDMETHOD.
  METHOD save_proxy_bypass.

    DATA:
      lt_textarea     TYPE TABLE OF string,
      lt_proxy_bypass TYPE Lif_abapgit_definitions=>ty_range_proxy_bypass_url,
      ls_proxy_bypass LIKE LINE OF lt_proxy_bypass.

    lt_textarea = Lcl_abapgit_convert=>split_string( mo_form_data->get( c_id-proxy_bypass ) ).

    ls_proxy_bypass-sign = 'I'.
    LOOP AT lt_textarea INTO ls_proxy_bypass-low WHERE table_line IS NOT INITIAL.
      IF ls_proxy_bypass-low CA '*+'.
        ls_proxy_bypass-option = 'CP'.
      ELSE.
        ls_proxy_bypass-option = 'EQ'.
      ENDIF.
      APPEND ls_proxy_bypass TO lt_proxy_bypass.
    ENDLOOP.

    mo_settings->set_proxy_bypass( lt_proxy_bypass ).

  ENDMETHOD.
  METHOD save_settings.

    DATA:
      li_persistence TYPE REF TO Lif_abapgit_persist_settings,
      lv_value       TYPE i.

    " Proxy
    mo_settings->set_proxy_url( mo_form_data->get( c_id-proxy_url ) ).
    mo_settings->set_proxy_port( mo_form_data->get( c_id-proxy_port ) ).
    mo_settings->set_proxy_authentication( boolc( mo_form_data->get( c_id-proxy_auth ) = abap_true ) ).

    save_proxy_bypass( ).

    " Commit Message
    lv_value = mo_form_data->get( c_id-commitmsg_comment_length ).
    mo_settings->set_commitmsg_comment_length( lv_value ).
    mo_settings->set_commitmsg_comment_default( mo_form_data->get( c_id-commitmsg_comment_deflt ) ).
    lv_value = mo_form_data->get( c_id-commitmsg_body_size ).
    mo_settings->set_commitmsg_body_size( lv_value ).
    mo_settings->set_commitmsg_hide_author( boolc( mo_form_data->get( c_id-commitmsg_hide_author ) = abap_true ) ).

    " Dev Internal
    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_false.
      mo_settings->set_run_critical_tests( boolc( mo_form_data->get( c_id-run_critical_tests ) = abap_true ) ).
      mo_settings->set_experimental_features( mo_form_data->get( c_id-experimental_features ) ).
    ENDIF.

    " Store in DB
    li_persistence = Lcl_abapgit_persist_factory=>get_settings( ).
    li_persistence->modify( mo_settings ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.
  METHOD validate_form.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    IF io_form_data->get( c_id-proxy_url ) IS NOT INITIAL AND io_form_data->get( c_id-proxy_port ) IS INITIAL OR
       io_form_data->get( c_id-proxy_url ) IS INITIAL AND io_form_data->get( c_id-proxy_port ) IS NOT INITIAL.
      ro_validation_log->set(
        iv_key = c_id-proxy_url
        iv_val = |If you specify a proxy, you have to specify host and port| ).
    ENDIF.

    IF ( io_form_data->get( c_id-proxy_url ) IS INITIAL OR io_form_data->get( c_id-proxy_port ) IS INITIAL ) AND
       io_form_data->get( c_id-proxy_auth ) = abap_true.
      ro_validation_log->set(
        iv_key = c_id-proxy_auth
        iv_val = |To turn on authentication, you have to specify host and port| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = Lcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-save.
        " Validate form entries before saving
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_GLOB implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SETT_INFO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_sett_infoccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_sett_infoccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SETT_INFO implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_sett_info.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Repository Stats'
      io_page_menu       = Lcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = Lif_abapgit_definitions=>c_action-repo_infos )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD format_size.

    DATA:
      lv_size TYPE p LENGTH 16 DECIMALS 2.

    IF iv_size > 1024 * 1024 * 1024.
      lv_size = iv_size / 1024 / 1024 / 1024.
      rv_size = |{ lv_size } GB|.
    ELSEIF iv_size > 1024 * 1024.
      lv_size = iv_size / 1024 / 1024.
      rv_size = |{ lv_size } MB|.
    ELSEIF iv_size > 1024.
      lv_size = iv_size / 1024.
      rv_size = |{ lv_size } KB|.
    ELSE.
      rv_size = |{ iv_size } Bytes|.
    ENDIF.

  ENDMETHOD.
  METHOD format_timestamp.

    DATA lv_short TYPE timestamp.

    IF iv_timestamp IS INITIAL.
      rv_timestamp = 'n/a'.
      RETURN.
    ENDIF.

    cl_abap_tstmp=>move(
      EXPORTING tstmp_src = iv_timestamp
      IMPORTING tstmp_tgt = lv_short ).

    rv_timestamp = |{ lv_short TIMESTAMP = ISO }|.

  ENDMETHOD.
  METHOD format_user.

    DATA lv_title TYPE string.

    IF iv_username IS INITIAL.
      rv_user = 'n/a'.
      RETURN.
    ENDIF.

    IF iv_username <> Lcl_abapgit_objects_super=>c_user_unknown.
      lv_title = Lcl_abapgit_user_record=>get_title( iv_username ).
    ENDIF.

    rv_user = iv_username.
    IF lv_title IS NOT INITIAL.
      rv_user = |{ rv_user } ({ lv_title })|.
    ENDIF.

  ENDMETHOD.
  METHOD get_form_schema.

    DATA lv_label TYPE string.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-infos-form'
                iv_help_page = 'https://docs.abapgit.org/settings-stats.html' ).

    IF mo_repo->is_offline( ) = abap_true.
      lv_label = 'ZIP File'.
    ELSE.
      lv_label = 'Remote'.
    ENDIF.

    ro_form->start_group(
      iv_name        = c_id-info
      iv_label       = 'Stats'
    )->text(
      iv_name        = c_id-created_by
      iv_label       = 'Created By'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-created_at
      iv_label       = 'Created At'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-deserialized_by
      iv_label       = 'Last Deserialized By'
      iv_readonly    = abap_true
    )->text(
      iv_name        = c_id-deserialized_at
      iv_label       = 'Last Deserialized At'
      iv_readonly    = abap_true
    )->table(
      iv_name        = c_id-stats_table
      iv_label       = 'Statistics'
    )->column(
      iv_label       = 'Measure'
      iv_width       = '50%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Local'
      iv_width       = '25%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = lv_label
      iv_width       = '25%'
      iv_readonly    = abap_true
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_settings.

    DATA:
      ls_repo  TYPE Lif_abapgit_persistence=>ty_repo,
      ls_stats TYPE ty_stats,
      lv_row   TYPE i,
      lv_int   TYPE i,
      lv_val   TYPE string.

    " Get infos from DB
    TRY.
        ls_repo = Lcl_abapgit_persist_factory=>get_repo( )->read( mo_repo->get_key( ) ).
      CATCH Lcx_abapgit_not_found.
        Lcx_abapgit_exception=>raise( |Repo not found, key { mo_repo->get_key( ) }| ).
    ENDTRY.

    read_stats( ).

    " Infos
    mo_form_data->set(
      iv_key = c_id-created_by
      iv_val = format_user( ls_repo-created_by ) ).
    mo_form_data->set(
      iv_key = c_id-created_at
      iv_val = format_timestamp( ls_repo-created_at ) ).
    mo_form_data->set(
      iv_key = c_id-deserialized_by
      iv_val = format_user( ls_repo-deserialized_by ) ).
    mo_form_data->set(
      iv_key = c_id-deserialized_at
      iv_val = format_timestamp( ls_repo-deserialized_at ) ).

    LOOP AT mt_stats INTO ls_stats.
      lv_row = sy-tabix.
      DO 3 TIMES.
        CASE sy-index.
          WHEN 1.
            lv_val = ls_stats-measure.
          WHEN 2.
            lv_val = ls_stats-local.
          WHEN 3.
            lv_val = ls_stats-remote.
        ENDCASE.

        IF ls_stats-measure CS 'Size' AND sy-index BETWEEN 2 AND 3.
          lv_int = lv_val.
          lv_val = format_size( lv_int ).
        ENDIF.

        mo_form_data->set(
          iv_key = |{ c_id-stats_table }-{ lv_row }-{ sy-index }|
          iv_val = lv_val ).
      ENDDO.
    ENDLOOP.

    mo_form_data->set(
      iv_key = |{ c_id-stats_table }-{ Lif_abapgit_html_form=>c_rows }|
      iv_val = |{ lv_row }| ).

  ENDMETHOD.
  METHOD read_stats.

    DATA:
      lt_local        TYPE Lif_abapgit_definitions=>ty_files_item_tt,
      lt_remote       TYPE Lif_abapgit_git_definitions=>ty_files_tt,
      lt_local_items  TYPE Lif_abapgit_definitions=>ty_items_tt,
      lt_remote_items TYPE Lif_abapgit_definitions=>ty_items_tt.

    CLEAR mt_stats.

    read_stats_files(
      IMPORTING
        et_local  = lt_local
        et_remote = lt_remote ).

    read_stats_state( ).

    read_stats_size_lines_sloc(
      EXPORTING
        it_local        = lt_local
        it_remote       = lt_remote
      IMPORTING
        et_local_items  = lt_local_items
        et_remote_items = lt_remote_items ).

    read_stats_objects(
      CHANGING
        ct_local_items  = lt_local_items
        ct_remote_items = lt_remote_items ).

  ENDMETHOD.
  METHOD read_stats_file.

    TYPES ty_char255 TYPE c LENGTH 255.

    DATA:
      lv_code TYPE string,
      lt_code TYPE STANDARD TABLE OF ty_char255 WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_code> LIKE LINE OF lt_code.

    rs_info-size = xstrlen( is_file-data ).

    IF is_file-filename CP '*.abap'.
      TRY.
          lv_code = Lcl_abapgit_convert=>xstring_to_string_utf8( is_file-data ).
        CATCH Lcx_abapgit_exception ##NO_HANDLER.
      ENDTRY.

      SPLIT lv_code AT cl_abap_char_utilities=>newline INTO TABLE lt_code.

      rs_info-line = lines( lt_code ).

      LOOP AT lt_code ASSIGNING <ls_code> WHERE table_line IS NOT INITIAL AND table_line(1) <> '*'.
        SHIFT <ls_code> LEFT DELETING LEADING space.
        IF <ls_code>(1) <> '"'.
          rs_info-sloc = rs_info-sloc + 1.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD read_stats_files.

    DATA ls_stats TYPE ty_stats.
    DATA lt_remote_wo_ignored TYPE Lif_abapgit_git_definitions=>ty_files_tt.

    et_local = mo_repo->get_files_local( ).

    ls_stats-measure = 'Number of Files'.
    ls_stats-local   = lines( et_local ).

    IF mo_repo->has_remote_source( ) = abap_true.
      et_remote = mo_repo->get_files_remote( ).
      ls_stats-remote = lines( et_remote ).
      lt_remote_wo_ignored = mo_repo->get_files_remote( iv_ignore_files = abap_true ).
    ENDIF.

    APPEND ls_stats TO mt_stats.

    IF et_remote IS NOT INITIAL.
      CLEAR ls_stats.
      ls_stats-measure = 'Number of Ignored Files'.
      ls_stats-remote = lines( et_remote ) - lines( lt_remote_wo_ignored ).
      APPEND ls_stats TO mt_stats.
    ENDIF.

  ENDMETHOD.
  METHOD read_stats_objects.

    DATA:
      ls_stats           TYPE ty_stats,
      ls_item            TYPE Lif_abapgit_definitions=>ty_item,
      lt_supported_types TYPE Lcl_abapgit_objects=>ty_types_tt.

    ls_stats-measure = 'Number of Objects'.

    DELETE ct_local_items WHERE obj_type IS INITIAL OR obj_name IS INITIAL.
    ls_stats-local = lines( ct_local_items ).

    DELETE ct_remote_items WHERE obj_type IS INITIAL OR obj_name IS INITIAL.
    ls_stats-remote = lines( ct_remote_items ).

    APPEND ls_stats TO mt_stats.

    CLEAR ls_stats.
    ls_stats-measure = 'Number of Unsupported Objects'.
    ls_stats-local   = lines( mo_repo->get_unsupported_objects_local( ) ).

    lt_supported_types = Lcl_abapgit_objects=>supported_list( ).

    LOOP AT ct_remote_items INTO ls_item.
      READ TABLE lt_supported_types WITH KEY table_line = ls_item-obj_type TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_stats-remote = ls_stats-remote + 1.
      ENDIF.
    ENDLOOP.

    APPEND ls_stats TO mt_stats.

  ENDMETHOD.
  METHOD read_stats_size_lines_sloc.

    DATA:
      ls_stats       TYPE ty_stats,
      lv_ignored     TYPE abap_bool,
      ls_info_file   TYPE ty_infos,
      ls_info_local  TYPE ty_infos,
      ls_info_remote TYPE ty_infos,
      ls_item        TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS:
      <ls_local>  LIKE LINE OF it_local,
      <ls_remote> LIKE LINE OF it_remote.

    LOOP AT it_local ASSIGNING <ls_local>.
      ls_info_file = read_stats_file( <ls_local>-file ).

      ls_info_local-size = ls_info_local-size + ls_info_file-size.
      ls_info_local-line = ls_info_local-line + ls_info_file-line.
      ls_info_local-sloc = ls_info_local-sloc + ls_info_file-sloc.

      COLLECT <ls_local>-item INTO et_local_items.
    ENDLOOP.

    IF mo_repo->has_remote_source( ) = abap_true.
      LOOP AT it_remote ASSIGNING <ls_remote> WHERE filename IS NOT INITIAL.
        lv_ignored = mo_repo->get_dot_abapgit( )->is_ignored(
                       iv_filename = <ls_remote>-filename
                       iv_path     = <ls_remote>-path ).

        IF lv_ignored = abap_false.
          ls_info_file = read_stats_file( <ls_remote> ).

          ls_info_remote-size = ls_info_remote-size + ls_info_file-size.
          ls_info_remote-line = ls_info_remote-line + ls_info_file-line.
          ls_info_remote-sloc = ls_info_remote-sloc + ls_info_file-sloc.

          TRY.
              Lcl_abapgit_filename_logic=>file_to_object(
                EXPORTING
                  iv_filename = <ls_remote>-filename
                  iv_path     = <ls_remote>-path
                  iv_devclass = mo_repo->get_package( )
                  io_dot      = mo_repo->get_dot_abapgit( )
                IMPORTING
                  es_item     = ls_item ).
              COLLECT ls_item INTO et_remote_items.
            CATCH Lcx_abapgit_exception ##NO_HANDLER.
          ENDTRY.
        ENDIF.

      ENDLOOP.
    ENDIF.

    ls_stats-measure = 'Size of Files'.
    ls_stats-local   = ls_info_local-size.
    ls_stats-remote  = ls_info_remote-size.
    APPEND ls_stats TO mt_stats.
    ls_stats-measure = 'Lines in ABAP Files'.
    ls_stats-local   = ls_info_local-line.
    ls_stats-remote  = ls_info_remote-line.
    APPEND ls_stats TO mt_stats.
    ls_stats-measure = 'Lines of Code in ABAP Files'.
    ls_stats-local   = ls_info_local-sloc.
    ls_stats-remote  = ls_info_remote-sloc.
    APPEND ls_stats TO mt_stats.

  ENDMETHOD.
  METHOD read_stats_state.

    DATA:
      lt_results TYPE Lif_abapgit_definitions=>ty_results_tt,
      lv_state   TYPE c LENGTH 1,
      ls_stats   TYPE ty_stats.

    FIELD-SYMBOLS:
      <ls_result> LIKE LINE OF lt_results.

    lt_results = Lcl_abapgit_repo_status=>calculate( mo_repo ).

    DO 3 TIMES.
      CLEAR ls_stats.

      CASE sy-index.
        WHEN 1.
          ls_stats-measure = 'Number of Modified Files'.
          lv_state = Lif_abapgit_definitions=>c_state-modified.
        WHEN 2.
          ls_stats-measure = 'Number of Added Files'.
          lv_state = Lif_abapgit_definitions=>c_state-added.
        WHEN 3.
          ls_stats-measure = 'Number of Deleted Files'.
          lv_state = Lif_abapgit_definitions=>c_state-deleted.
      ENDCASE.

      LOOP AT lt_results ASSIGNING <ls_result>.
        IF <ls_result>-lstate = lv_state.
          ls_stats-local = ls_stats-local + 1.
        ENDIF.
        IF <ls_result>-rstate = lv_state AND mo_repo->has_remote_source( ) = abap_true.
          ls_stats-remote = ls_stats-remote + 1.
        ENDIF.
      ENDLOOP.

      APPEND ls_stats TO mt_stats.
    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    IF ii_event->mv_action = Lif_abapgit_definitions=>c_action-go_back.
      rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    read_settings( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render( mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SETT_INFO implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_SYNTAX <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_syntax===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_syntax===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_SYNTAX implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
    run_syntax_check( ).
  ENDMETHOD.
  METHOD run_syntax_check.

    DATA: li_syntax_check TYPE REF TO Lif_abapgit_code_inspector.

    li_syntax_check = Lcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) ).

    TRY.
        mt_result = li_syntax_check->run( c_variant ).
      CATCH Lcx_abapgit_exception.
        " Variant SYNTAX_CHECK does not exist in 702
        mt_result = li_syntax_check->run( 'VERI_' && c_variant ).
    ENDTRY.

    mv_summary = li_syntax_check->get_summary( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_syntax.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Syntax Check'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-rerun.

        run_syntax_check( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        rs_handled = on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Syntax Check'.

    ls_hotkey_action-description = |Re-Run|.
    ls_hotkey_action-action = c_actions-rerun.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = build_base_menu( ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top( io_repo        = mo_repo
                                                              iv_show_commit = abap_false ) ).
    ri_html->add( `</div>` ).

    ri_html->add( '<div class="toc">' ).

    ri_html->add( render_variant(
      iv_variant = c_variant
      iv_summary = mv_summary ) ).

    IF lines( mt_result ) = 0.
      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No syntax errors' ).
      ri_html->add( '</div>' ).
    ELSE.
      render_result( ii_html   = ri_html
                     it_result = mt_result ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_SYNTAX implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_TUTORIAL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_tutorial=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_tutorial=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_TUTORIAL implementation.
*"* method's implementations
*include methods.
  METHOD build_main_menu.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.

    ro_menu->add(
      iv_txt = Lcl_abapgit_gui_buttons=>repo_list( )
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>new_online( )
      iv_act = Lif_abapgit_definitions=>c_action-repo_newonline
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>new_offline( )
      iv_act = Lif_abapgit_definitions=>c_action-repo_newoffline
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>help( )
      io_sub = Lcl_abapgit_gui_menus=>help( ) ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_tutorial.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Tutorial'
      io_page_menu       = build_main_menu( )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="tutorial">' ).

    ri_html->add( '<h1>Tutorial</h1>' ).
    ri_html->add( '<hr>' ).

    ri_html->add( '<h2>Online repositories</h2>' ).
    ri_html->add( '<p><ul>' ).

    ri_html->add( `<li>To clone a remote repository (e.g. from github) click ` ).
    ri_html->add_a( iv_txt = Lcl_abapgit_gui_buttons=>new_online( )
                    iv_act = Lif_abapgit_definitions=>c_action-repo_newonline ).
    ri_html->add( ' from the top menu. This will link a remote repository with a package on your system.</li>' ).
    ri_html->add( '<li>Use the pull button to retrieve and activate the remote objects.</li>' ).
    ri_html->add( '<li>If the remote repository is updated,' ).
    ri_html->add( ' you will see the changes and can pull to apply the updates.</li>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '<h2>Offline repositories</h2>' ).
    ri_html->add( '<p><ul>' ).

    ri_html->add( `<li>To add a package as an offline repository, click ` ).
    ri_html->add_a( iv_txt = Lcl_abapgit_gui_buttons=>new_offline( )
                    iv_act = Lif_abapgit_definitions=>c_action-repo_newoffline ).
    ri_html->add( ' from the top menu.' ).
    ri_html->add( '<li>abapGit will start tracking changes for the package ' ).
    ri_html->add( 'without linking it to an online git repository.</li>' ).
    ri_html->add( '<li>You can link the package later or just export the package content as a ZIP file.</li>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '</ul></p>' ).

    ri_html->add( '<h2>Repository list and favorites</h2>' ).
    ri_html->add( '<p><ul>' ).
    ri_html->add( |<li>To favorite a repository, use the {
                  ri_html->icon( 'star/darkgrey' ) } icon in the repository list.</li>| ).
    ri_html->add( |<li>To go to a repository, click on the repository name.</li>| ).
    ri_html->add( |<li>To go back to your favorites, use the| ).
    ri_html->add_a(
      iv_txt = Lcl_abapgit_gui_buttons=>repo_list( )
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home ).
    ri_html->add( |</li>| ).

    ri_html->add( `<li>` ).
    ri_html->add_a( iv_txt = 'Explore'
                    iv_act = Lif_abapgit_definitions=>c_action-go_explore ).
    ri_html->add( ' to find projects using abapGit</li>' ).


    ri_html->add( '</ul></p>' ).
    ri_html->add( '</div>' ).


  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_TUTORIAL implementation

*>>>>>>> ZCL_ABAPGIT_HTML <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html==============ccau.
CLASS SHRITEFUH64VYIPO5IWUYB3KW2JSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRITEFUH64VYIPO5IWUYB3KW2JSFY IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
    ri_html = Lcl_abapgit_html=>create( 'Hello' ).
  ENDMETHOD.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW2LSFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_gui_renderable.
ENDCLASS.
CLASS SHRITEFUH64VYIPO5IWUYB3KW2LSFY IMPLEMENTATION.
  METHOD Lif_abapgit_gui_renderable~render.
    Lcx_abapgit_exception=>raise( 'Fail!' ).
  ENDMETHOD.
ENDCLASS.




class LCL_ABAPGIT_HTML implementation.
*"* method's implementations
*include methods.
  METHOD checkbox.

    DATA: lv_checked TYPE string.

    IF iv_checked = abap_true.
      lv_checked = |checked|.
    ENDIF.

    rv_html = |<input type="checkbox" { lv_checked } |.
    IF iv_id IS NOT INITIAL.
      rv_html = rv_html && |id="{ iv_id }"|.
    ENDIF.

    rv_html = rv_html && `/>`.

  ENDMETHOD.
  METHOD class_constructor.

    DATA lv_mode TYPE tabname.

    CREATE OBJECT go_single_tags_re
      EXPORTING
        pattern     = '<(AREA|BASE|BR|COL|COMMAND|EMBED|HR|IMG|INPUT|LINK|META|PARAM|SOURCE|!)'
        ignore_case = abap_false.

    gv_spaces = repeat(
      val = ` `
      occ = c_max_indent ).

    GET PARAMETER ID 'DBT' FIELD lv_mode.
    gv_debug_mode = boolc( lv_mode = 'HREF' ).

  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ri_instance TYPE Lcl_abapgit_html.
    IF iv_initial_chunk IS NOT INITIAL.
      ri_instance->add( iv_initial_chunk ).
    ENDIF.
  ENDMETHOD.
  METHOD icon.

    DATA: lv_hint       TYPE string,
          lv_name       TYPE string,
          lv_color      TYPE string,
          lv_class      TYPE string,
          lv_large_icon TYPE string,
          lv_xpixel     TYPE i,
          lv_onclick    TYPE string.

    SPLIT iv_name AT '/' INTO lv_name lv_color.

    IF iv_hint IS NOT INITIAL.
      lv_hint  = | title="{ iv_hint }"|.
    ENDIF.
    IF iv_onclick IS NOT INITIAL.
      lv_onclick = | onclick="{ iv_onclick }"|.
    ENDIF.
    IF iv_class IS NOT INITIAL.
      lv_class = | { iv_class }|.
    ENDIF.
    IF lv_color IS NOT INITIAL.
      lv_color = | { lv_color }|.
    ENDIF.

    " Automatic icon scaling (could be overwritten by personal setting)
    " see zcl_abapgit_gui_page->html_head
    lv_xpixel = cl_gui_cfw=>compute_pixel_from_metric( x_or_y = 'X'
                                                       in = 1 ).
    IF lv_xpixel >= 2.
      lv_large_icon = ' large'.
    ENDIF.

    rv_str = |<i class="icon{ lv_large_icon } icon-{ lv_name }{ lv_color }|.
    rv_str = |{ rv_str }{ lv_class }"{ lv_onclick }{ lv_hint }></i>|.

  ENDMETHOD.
  METHOD indent_line.

    DATA: ls_study  TYPE ty_study_result,
          lv_spaces TYPE i.

    ls_study = study_line(
      is_context = cs_context
      iv_line    = cv_line ).

    " No indent for textarea tags
    IF ls_study-textarea_open = abap_true.
      cs_context-within_textarea = abap_true.
      RETURN.
    ELSEIF ls_study-textarea_close = abap_true.
      cs_context-within_textarea = abap_false.
      RETURN.
    ELSEIF cs_context-within_textarea = abap_true.
      RETURN.
    ENDIF.

    " First closing tag - shift back exceptionally
    IF ( ls_study-script_close = abap_true
        OR ls_study-style_close = abap_true
        OR ls_study-curly_close = abap_true
        OR ls_study-tag_close = abap_true )
        AND cs_context-indent > 0.
      lv_spaces = ( cs_context-indent - 1 ) * c_indent_size.
      IF lv_spaces <= c_max_indent.
        cv_line  = gv_spaces(lv_spaces) && cv_line.
      ELSE.
        cv_line = gv_spaces && cv_line.
      ENDIF.
    ELSE.
      cv_line = cs_context-indent_str && cv_line.
    ENDIF.

    " Context status update
    CASE abap_true.
      WHEN ls_study-script_open.
        cs_context-within_js    = abap_true.
        cs_context-within_style = abap_false.
      WHEN ls_study-style_open.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_true.
      WHEN ls_study-script_close OR ls_study-style_close.
        cs_context-within_js    = abap_false.
        cs_context-within_style = abap_false.
        ls_study-closings       = ls_study-closings + 1.
    ENDCASE.

    " More-less logic chosen due to possible double tags in a line '<a><b>'
    IF ls_study-openings <> ls_study-closings.
      IF ls_study-openings > ls_study-closings.
        cs_context-indent = cs_context-indent + 1.
      ELSEIF cs_context-indent > 0. " AND ls_study-openings < ls_study-closings
        cs_context-indent = cs_context-indent - 1.
      ENDIF.
      lv_spaces = cs_context-indent * c_indent_size.
      IF lv_spaces <= c_max_indent.
        cs_context-indent_str = gv_spaces(lv_spaces).
      ELSE.
        cv_line = gv_spaces && cv_line.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD study_line.

    DATA: lv_line TYPE string,
          lv_len  TYPE i.

    lv_line = to_upper( shift_left( val = iv_line
                                    sub = ` ` ) ).
    lv_len  = strlen( lv_line ).

    " Some assumptions for simplification and speed
    " - style & scripts tag should be opened/closed in a separate line
    " - style & scripts opening and closing in one line is possible but only once

    " TODO & Issues
    " - What if the string IS a well formed html already not just single line ?

    IF is_context-within_js = abap_true OR is_context-within_style = abap_true.

      IF is_context-within_js = abap_true AND lv_len >= 8 AND lv_line(8) = '</SCRIPT'.
        rs_result-script_close = abap_true.
      ELSEIF is_context-within_style = abap_true AND lv_len >= 7 AND lv_line(7) = '</STYLE'.
        rs_result-style_close = abap_true.
      ENDIF.

      IF is_context-no_indent_jscss = abap_false.
        IF lv_len >= 1 AND lv_line(1) = '}'.
          rs_result-curly_close = abap_true.
        ENDIF.

        FIND ALL OCCURRENCES OF '{' IN lv_line MATCH COUNT rs_result-openings.
        FIND ALL OCCURRENCES OF '}' IN lv_line MATCH COUNT rs_result-closings.
      ENDIF.

    ELSE.
      IF lv_len >= 7 AND lv_line(7) = '<SCRIPT'.
        FIND FIRST OCCURRENCE OF '</SCRIPT' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-script_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 6 AND lv_line(6) = '<STYLE'.
        FIND FIRST OCCURRENCE OF '</STYLE' IN lv_line.
        IF sy-subrc > 0. " Not found
          rs_result-style_open = abap_true.
        ENDIF.
      ENDIF.
      IF lv_len >= 2 AND lv_line(2) = '</'.
        rs_result-tag_close = abap_true.
      ENDIF.

      FIND ALL OCCURRENCES OF '<'  IN lv_line MATCH COUNT rs_result-openings.
      FIND ALL OCCURRENCES OF '</' IN lv_line MATCH COUNT rs_result-closings.
      IF rs_result-closings <> rs_result-openings.
* if everything is closings, there are no single tags
        FIND ALL OCCURRENCES OF REGEX go_single_tags_re IN lv_line MATCH COUNT rs_result-singles.
      ENDIF.
      rs_result-openings = rs_result-openings - rs_result-closings - rs_result-singles.

    ENDIF.

    " Textarea (same assumptions as above)
    IF is_context-within_textarea = abap_true AND lv_len >= 10 AND lv_line(10) = '</TEXTAREA'.
      rs_result-textarea_close = abap_true.
    ELSEIF is_context-within_textarea = abap_false AND lv_len >= 9 AND lv_line(9) = '<TEXTAREA'.
      FIND FIRST OCCURRENCE OF '</TEXTAREA' IN lv_line.
      IF sy-subrc > 0. " Not found
        rs_result-textarea_open = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_html~a.

    DATA: lv_class TYPE string,
          lv_href  TYPE string,
          lv_click TYPE string,
          lv_id    TYPE string,
          lv_act   TYPE string,
          lv_style TYPE string,
          lv_title TYPE string.

    lv_class = iv_class.

    IF iv_opt CA Lif_abapgit_html=>c_html_opt-strong.
      lv_class = lv_class && ' emphasis'.
    ENDIF.
    IF iv_opt CA Lif_abapgit_html=>c_html_opt-cancel.
      lv_class = lv_class && ' attention'.
    ENDIF.
    IF iv_opt CA Lif_abapgit_html=>c_html_opt-crossout.
      lv_class = lv_class && ' crossout grey'.
    ENDIF.
    IF lv_class IS NOT INITIAL.
      SHIFT lv_class LEFT DELETING LEADING space.
      lv_class = | class="{ lv_class }"|.
    ENDIF.

    lv_href = ' href="#"'. " Default, dummy
    lv_act  = iv_act.
    IF ( iv_act IS NOT INITIAL OR iv_typ = Lif_abapgit_html=>c_action_type-dummy )
        AND iv_opt NA Lif_abapgit_html=>c_html_opt-crossout.
      CASE iv_typ.
        WHEN Lif_abapgit_html=>c_action_type-url.
          IF iv_query IS NOT INITIAL.
            lv_act = lv_act && `?` && iv_query.
          ENDIF.
          lv_href  = | href="{ lv_act }"|.
        WHEN Lif_abapgit_html=>c_action_type-sapevent.
          IF iv_query IS NOT INITIAL.
            lv_act = lv_act && `?` && iv_query.
          ENDIF.
          lv_href  = | href="sapevent:{ lv_act }"|.
        WHEN Lif_abapgit_html=>c_action_type-onclick.
          lv_href  = ' href="#"'.
          lv_click = | onclick="{ iv_act }"|.
        WHEN Lif_abapgit_html=>c_action_type-dummy.
          lv_href  = ' href="#"'.
      ENDCASE.
    ENDIF.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_style IS NOT INITIAL.
      lv_style = | style="{ iv_style }"|.
    ENDIF.

    IF iv_title IS NOT INITIAL.
      lv_title = | title="{ iv_title }"|.
    ENDIF.

    " Debug option to display href-link on hover
    IF gv_debug_mode = abap_true.
      lv_title = | title="{ escape(
        val    = lv_href
        format = cl_abap_format=>e_html_attr ) }"|.
    ENDIF.

    rv_str = |<a{ lv_id }{ lv_class }{ lv_href }{ lv_click }{ lv_style }{ lv_title }>|
          && |{ iv_txt }</a>|.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add.

    DATA: lv_type       TYPE c,
          li_renderable TYPE REF TO Lif_abapgit_gui_renderable,
          lx_error      TYPE REF TO Lcx_abapgit_exception,
          lo_html       TYPE REF TO Lcl_abapgit_html.

    FIELD-SYMBOLS: <lt_tab> TYPE string_table.

    lv_type = cl_abap_typedescr=>describe_by_data( ig_chunk )->type_kind.

    CASE lv_type.
      WHEN 'C' OR 'g'.  " Char or string
        APPEND ig_chunk TO mt_buffer.
      WHEN 'h'.         " Table
        ASSIGN ig_chunk TO <lt_tab>. " Assuming table of strings ! Will dump otherwise
        APPEND LINES OF <lt_tab> TO mt_buffer.
      WHEN 'r'.         " Object ref
        ASSERT ig_chunk IS BOUND. " Dev mistake
        TRY.
            lo_html ?= ig_chunk.
          CATCH cx_sy_move_cast_error.
            TRY.
                li_renderable ?= ig_chunk.
                lo_html ?= li_renderable->render( ).
              CATCH cx_sy_move_cast_error.
                ASSERT 1 = 0. " Dev mistake
              CATCH Lcx_abapgit_exception INTO lx_error.
                lo_html ?= create( |<span class="error">Render error: { lx_error->get_text( ) }</span>| ).
            ENDTRY.
        ENDTRY.
        APPEND LINES OF lo_html->mt_buffer TO mt_buffer.
      WHEN OTHERS.
        ASSERT 1 = 0. " Dev mistake
    ENDCASE.

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add_a.

    Lif_abapgit_html~add( Lif_abapgit_html~a(
      iv_txt   = iv_txt
      iv_act   = iv_act
      iv_query = iv_query
      iv_typ   = iv_typ
      iv_opt   = iv_opt
      iv_class = iv_class
      iv_id    = iv_id
      iv_style = iv_style
      iv_title = iv_title ) ).

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add_checkbox.

    Lif_abapgit_html~add( checkbox(
      iv_id      = iv_id
      iv_checked = iv_checked ) ).

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~add_icon.

    Lif_abapgit_html~add( icon(
      iv_name    = iv_name
      iv_class   = iv_class
      iv_hint    = iv_hint
      iv_onclick = iv_onclick ) ).

    ri_self = me.

  ENDMETHOD.
  METHOD Lif_abapgit_html~icon.

    rv_str = icon(
      iv_name    = iv_name
      iv_hint    = iv_hint
      iv_class   = iv_class
      iv_onclick = iv_onclick ).

  ENDMETHOD.
  METHOD Lif_abapgit_html~is_empty.
    rv_yes = boolc( lines( mt_buffer ) = 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_html~render.

    DATA: ls_context TYPE ty_indent_context,
          lt_temp    TYPE string_table.

    FIELD-SYMBOLS: <lv_line>   LIKE LINE OF lt_temp,
                   <lv_line_c> LIKE LINE OF lt_temp.

    ls_context-no_indent_jscss = iv_no_indent_jscss.

    LOOP AT mt_buffer ASSIGNING <lv_line>.
      APPEND <lv_line> TO lt_temp ASSIGNING <lv_line_c>.
      indent_line( CHANGING cs_context = ls_context cv_line = <lv_line_c> ).
    ENDLOOP.

    CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.
  METHOD Lif_abapgit_html~set_title.
    Lif_abapgit_html~mv_chunk_title = iv_title.
    ri_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_html~td.
    Lif_abapgit_html~wrap(
      iv_format_single_line = iv_format_single_line
      iv_tag   = 'td'
      iv_content = iv_content
      ii_content = ii_content
      iv_id    = iv_id
      iv_class = iv_class
      iv_hint  = iv_hint ).
    ri_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_html~th.
    Lif_abapgit_html~wrap(
      iv_format_single_line = iv_format_single_line
      iv_tag   = 'th'
      iv_content = iv_content
      ii_content = ii_content
      iv_id    = iv_id
      iv_class = iv_class
      iv_hint  = iv_hint ).
    ri_self = me.
  ENDMETHOD.
  METHOD Lif_abapgit_html~wrap.

    DATA lv_open_tag TYPE string.
    DATA lv_close_tag TYPE string.

    DATA: lv_class TYPE string,
          lv_id    TYPE string,
          lv_title TYPE string.

    IF iv_id IS NOT INITIAL.
      lv_id = | id="{ iv_id }"|.
    ENDIF.

    IF iv_class IS NOT INITIAL.
      lv_class = | class="{ iv_class }"|.
    ENDIF.

    IF iv_hint IS NOT INITIAL.
      lv_title = | title="{ iv_hint }"|.
    ENDIF.

    lv_open_tag = |<{ iv_tag }{ lv_id }{ lv_class }{ lv_title }>|.
    lv_close_tag = |</{ iv_tag }>|.

    IF ii_content IS NOT BOUND AND iv_content IS INITIAL.
      lv_open_tag = lv_open_tag && lv_close_tag.
      CLEAR lv_close_tag.
    ENDIF.

    IF iv_format_single_line = abap_true AND iv_content IS NOT INITIAL.
      Lif_abapgit_html~add( lv_open_tag && iv_content && lv_close_tag ).
    ELSE.
      Lif_abapgit_html~add( lv_open_tag ).
      IF ii_content IS BOUND.
        Lif_abapgit_html~add( ii_content ).
      ELSEIF iv_content IS NOT INITIAL.
        Lif_abapgit_html~add( iv_content ).
      ENDIF.
      IF lv_close_tag IS NOT INITIAL.
        Lif_abapgit_html~add( lv_close_tag ).
      ENDIF.
    ENDIF.

    ri_self = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML implementation

*>>>>>>> ZCL_ABAPGIT_HTML_TABLE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_table========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_table========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_html_table========ccau.


class LCL_ABAPGIT_HTML_TABLE implementation.
*"* method's implementations
*include methods.
  METHOD create.
    ASSERT ii_renderer IS BOUND.
    CREATE OBJECT ro_instance.
    ro_instance->mi_renderer = ii_renderer.
  ENDMETHOD.
  METHOD define_column.

    FIELD-SYMBOLS <ls_c> LIKE LINE OF mt_columns.

    ASSERT iv_column_id IS NOT INITIAL.
    ro_self = me.

    APPEND INITIAL LINE TO mt_columns ASSIGNING <ls_c>.
    <ls_c>-column_id    = iv_column_id.
    <ls_c>-column_title = iv_column_title.
    <ls_c>-from_field   = to_upper( iv_from_field ).

  ENDMETHOD.
  METHOD render.

    DATA lv_attrs TYPE string.

    IF iv_id IS NOT INITIAL.
      lv_attrs = lv_attrs && | id="{ iv_id }"|.
    ENDIF.

    IF iv_css_class IS NOT INITIAL.
      lv_attrs = lv_attrs && | class="{ iv_css_class }"|.
    ENDIF.

    CREATE OBJECT mi_html TYPE Lcl_abapgit_html.
    ri_html = mi_html.

    mi_html->add( |<table{ lv_attrs }>| ).
    render_thead( ).
    render_tbody( it_data ).
    mi_html->add( '</table>' ).

  ENDMETHOD.
  METHOD render_row.

    DATA ls_render TYPE Lif_abapgit_html_table=>ty_cell_render.
    DATA lv_dummy TYPE string.
    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.
    FIELD-SYMBOLS <lv_val> TYPE any.

    LOOP AT mt_columns ASSIGNING <ls_col>.
      IF <ls_col>-from_field IS NOT INITIAL AND <ls_col>-from_field <> '-'.
        ASSIGN COMPONENT <ls_col>-from_field OF STRUCTURE is_row TO <lv_val>.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise( |html_table: cannot assign field [{ <ls_col>-from_field }]| ).
        ENDIF.
      ELSEIF <ls_col>-from_field <> '-'.
        <ls_col>-from_field = to_upper( <ls_col>-column_id ). " Try column_id
        ASSIGN COMPONENT <ls_col>-from_field OF STRUCTURE is_row TO <lv_val>.
        IF sy-subrc <> 0.
          <ls_col>-from_field = '-'. " Don't try assignments anymore
          ASSIGN lv_dummy TO <lv_val>.
        ENDIF.
      ELSE.
        ASSIGN lv_dummy TO <lv_val>.
      ENDIF.
      ls_render = mi_renderer->render_cell(
        iv_row_index = iv_row_index
        is_row       = is_row
        iv_column_id = <ls_col>-column_id
        iv_value     = <lv_val> ).
      mi_html->td(
        iv_content = ls_render-content
        ii_content = ls_render-html
        iv_class   = ls_render-css_class ).
    ENDLOOP.

  ENDMETHOD.
  METHOD render_tbody.

    DATA ls_row_attrs TYPE Lif_abapgit_html_table=>ty_row_attrs.
    DATA lv_row_attrs TYPE string.
    DATA lv_index TYPE i.

    FIELD-SYMBOLS <ls_i> TYPE any.

    mi_html->add( '<tbody>' ).

    LOOP AT it_data ASSIGNING <ls_i>.
      lv_index = sy-tabix.
      ls_row_attrs = mi_renderer->get_row_attrs(
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      CLEAR lv_row_attrs.
      IF ls_row_attrs-css_class IS NOT INITIAL.
        lv_row_attrs = lv_row_attrs && | class="{ ls_row_attrs-css_class }"|.
      ENDIF.
      mi_html->add( |<tr{ lv_row_attrs }>| ).
      render_row(
        iv_row_index = lv_index
        is_row       = <ls_i> ).
      mi_html->add( '</tr>' ).
    ENDLOOP.

    mi_html->add( '</tbody>' ).

  ENDMETHOD.
  METHOD render_thead.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF mt_columns.

    mi_html->add( '<thead>' ).
    mi_html->add( '<tr>' ).

    LOOP AT mt_columns ASSIGNING <ls_col>.
      mi_html->th( <ls_col>-column_title ).
    ENDLOOP.

    mi_html->add( '</tr>' ).
    mi_html->add( '</thead>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_TABLE implementation

*>>>>>>> ZCL_ABAPGIT_HTML_TOOLBAR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_html_toolbar======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_html_toolbar======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTML_TOOLBAR implementation.
*"* method's implementations
*include methods.
  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_typ = Lif_abapgit_html=>c_action_type-separator  " sep doesn't have action
      OR iv_typ = Lif_abapgit_html=>c_action_type-onclick      " click may have no action (assigned in JS)
      OR iv_typ = Lif_abapgit_html=>c_action_type-dummy        " dummy may have no action
      OR iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ASSERT NOT ( iv_chk <> abap_undefined AND io_sub IS NOT INITIAL ).

    ls_item-txt   = iv_txt.
    ls_item-act   = iv_act.
    ls_item-ico   = iv_ico.
    ls_item-sub   = io_sub.
    ls_item-opt   = iv_opt.
    ls_item-typ   = iv_typ.
    ls_item-cur   = iv_cur.
    ls_item-chk   = iv_chk.
    ls_item-aux   = iv_aux.
    ls_item-id    = iv_id.
    ls_item-title = iv_title.
    ls_item-class = iv_class.
    ls_item-li_class = iv_li_class.

    APPEND ls_item TO mt_items.

    ro_self = me.

  ENDMETHOD.
  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.
  METHOD count_items.
    rv_count = lines( mt_items ).
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_id = iv_id.
  ENDMETHOD.
  METHOD render.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_class = 'nav-container'.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_as_droplist.

    DATA: lv_class TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_class = 'nav-container'.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.
    IF iv_corner = abap_true.
      lv_class = lv_class && ' corner'.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( '<ul><li>' ).
    ri_html->add_a( iv_txt = iv_label
                    iv_typ = Lif_abapgit_html=>c_action_type-sapevent
                    iv_act = iv_action ).
    ri_html->add( '<div class="minizone"></div>' ).
    ri_html->add( render_items( iv_sort = iv_sort ) ).
    ri_html->add( '</li></ul>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_items.

    DATA: lv_class       TYPE string,
          lv_class_value TYPE string,
          lv_icon        TYPE string,
          lv_id          TYPE string,
          lv_check       TYPE string,
          lv_aux         TYPE string,
          lv_has_icons   TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.


    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    " Check has icons or check boxes
    LOOP AT mt_items ASSIGNING <ls_item> WHERE ico IS NOT INITIAL OR chk <> abap_undefined.
      lv_has_icons = abap_true.
      lv_class     = ' class="with-icons"'.
      EXIT.
    ENDLOOP.

    IF mv_id IS NOT INITIAL.
      lv_id = | id="{ mv_id }"|.
    ENDIF.

    ri_html->add( |<ul{ lv_id }{ lv_class }>| ).

    " Render items
    LOOP AT mt_items ASSIGNING <ls_item>.
      CLEAR: lv_class, lv_class_value, lv_icon.

      IF <ls_item>-typ = Lif_abapgit_html=>c_action_type-separator.
        ri_html->add( |<li class="separator">{ <ls_item>-txt }</li>| ).
        CONTINUE.
      ENDIF.

      IF lv_has_icons = abap_true.
        IF <ls_item>-chk = abap_true.
          lv_icon  = ri_html->icon( 'check/blue' ).
          lv_check = ' data-check="X"'.
        ELSEIF <ls_item>-chk = abap_false.
          lv_icon = ri_html->icon( 'check/grey' ).
          lv_check = ' data-check=""'.
        ELSE. " abap_undefined -> not a check box
          lv_icon = ri_html->icon( <ls_item>-ico ).
        ENDIF.
      ENDIF.


      IF <ls_item>-cur = abap_true.
        IF <ls_item>-li_class IS INITIAL.
          lv_class_value = 'current-menu-item'.
        ELSE.
          lv_class_value = |current-menu-item { <ls_item>-li_class }|.
        ENDIF.
      ELSE.
        lv_class_value = <ls_item>-li_class.
      ENDIF.
      IF lv_class_value IS NOT INITIAL.
        lv_class = | class="{ lv_class_value }"|.
      ENDIF.
      IF <ls_item>-aux IS NOT INITIAL.
        lv_aux = | data-aux="{ <ls_item>-aux }"|.
      ENDIF.

      ri_html->add( |<li{ lv_class }{ lv_check }{ lv_aux }>| ).

      IF <ls_item>-sub IS INITIAL.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = <ls_item>-typ
                        iv_act   = <ls_item>-act
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title
                        iv_class = <ls_item>-class ).
      ELSE.
        ri_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = Lif_abapgit_html=>c_action_type-dummy
                        iv_act   = ''
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt
                        iv_title = <ls_item>-title
                        iv_class = <ls_item>-class ).
        ri_html->add( <ls_item>-sub->render_items( iv_sort ) ).
      ENDIF.
      ri_html->add( '</li>' ).

    ENDLOOP.

    ri_html->add( '</ul>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTML_TOOLBAR implementation

*>>>>>>> ZCL_ABAPGIT_HTTP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http==============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http==============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTTP implementation.
*"* method's implementations
*include methods.
  METHOD acquire_login_details.

    DATA: lv_default_user TYPE string,
          lv_user         TYPE string,
          lv_pass         TYPE string,
          lo_digest       TYPE REF TO Lcl_abapgit_http_digest.


    lv_default_user = Lcl_abapgit_persistence_user=>get_instance( )->get_repo_login( iv_url ).
    lv_user         = lv_default_user.

    Lcl_abapgit_password_dialog=>popup(
      EXPORTING
        iv_repo_url     = iv_url
      CHANGING
        cv_user         = lv_user
        cv_pass         = lv_pass ).

    IF lv_user IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Unauthorized access. Check your credentials' ).
    ENDIF.

    IF lv_user <> lv_default_user.
      Lcl_abapgit_persistence_user=>get_instance( )->set_repo_login(
        iv_url   = iv_url
        iv_login = lv_user ).
    ENDIF.

    rv_scheme = ii_client->response->get_header_field( 'www-authenticate' ).
    FIND REGEX '^(\w+)' IN rv_scheme SUBMATCHES rv_scheme.

    CASE rv_scheme.
      WHEN c_scheme-digest.
* https://en.wikipedia.org/wiki/Digest_access_authentication
* e.g. used by https://www.gerritcodereview.com/
        CREATE OBJECT lo_digest
          EXPORTING
            ii_client   = ii_client
            iv_username = lv_user
            iv_password = lv_pass.
        lo_digest->run( ii_client ).
        io_client->set_digest( lo_digest ).
      WHEN OTHERS.
* https://en.wikipedia.org/wiki/Basic_access_authentication
        ii_client->authenticate(
          username = lv_user
          password = lv_pass ).
    ENDCASE.

  ENDMETHOD.
  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status( IMPORTING code = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD create_by_url.

    DATA: lv_uri                 TYPE string,
          lv_scheme              TYPE string,
          lv_authorization       TYPE string,
          li_client              TYPE REF TO if_http_client,
          ls_header              LIKE LINE OF it_headers,
          lo_proxy_configuration TYPE REF TO Lcl_abapgit_proxy_config,
          lv_text                TYPE string.


    CREATE OBJECT lo_proxy_configuration.

    li_client = Lcl_abapgit_exit=>get_instance( )->create_http_client( iv_url ).

    IF li_client IS NOT BOUND.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = Lcl_abapgit_url=>host( iv_url )
          ssl_id             = Lcl_abapgit_exit=>get_instance( )->get_ssl_id( )
          proxy_host         = lo_proxy_configuration->get_proxy_url( iv_url )
          proxy_service      = lo_proxy_configuration->get_proxy_port( iv_url )
        IMPORTING
          client             = li_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            " make sure:
            " a) SSL is setup properly in STRUST
            lv_text = 'HTTPS ARGUMENT_NOT_FOUND | STRUST/SSL Setup correct?'.
          WHEN OTHERS.
            lv_text = 'While creating HTTP Client'.

        ENDCASE.
        Lcx_abapgit_exception=>raise( lv_text ).
      ENDIF.

    ENDIF.

    IF lo_proxy_configuration->get_proxy_authentication( iv_url ) = abap_true.
      Lcl_abapgit_proxy_auth=>run( li_client ).
    ENDIF.

    CREATE OBJECT ro_client
      EXPORTING
        ii_client = li_client.

    IF is_local_system( iv_url ) = abap_true.
      li_client->send_sap_logon_ticket( ).
    ENDIF.

    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    li_client->request->set_header_field(
        name  = 'user-agent'
        value = get_agent( ) ).
    lv_uri = Lcl_abapgit_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    LOOP AT it_headers INTO ls_header.
      li_client->request->set_header_field(
        name  = ls_header-key
        value = ls_header-value ).
    ENDLOOP.

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lv_authorization = Lcl_abapgit_login_manager=>load( iv_url ).
    IF lv_authorization IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'authorization'
        value = lv_authorization ).
      li_client->propertytype_logon_popup = li_client->co_disabled.
    ENDIF.

    Lcl_abapgit_exit=>get_instance( )->http_client(
      iv_url    = iv_url
      ii_client = li_client ).

    ro_client->send_receive( ).
    IF check_auth_requested( li_client ) = abap_true.
      lv_scheme = acquire_login_details( ii_client = li_client
                                         io_client = ro_client
                                         iv_url    = iv_url ).
      ro_client->send_receive( ).
    ENDIF.
    ro_client->check_http_200( ).

    IF lv_scheme <> c_scheme-digest.
      Lcl_abapgit_login_manager=>save(
        iv_uri           = iv_url
        iv_authorization = li_client->request->get_header_field( 'authorization' ) ).
    ENDIF.

  ENDMETHOD.
  METHOD get_agent.

* bitbucket require agent prefix = "git/"
* also see https://github.com/abapGit/abapGit/issues/1432
    rv_agent = |git/2.0 (abapGit { Lif_abapgit_version=>c_abap_version })|.

  ENDMETHOD.
  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE Lif_abapgit_definitions=>ty_string_tt,
          li_exit TYPE REF TO Lif_abapgit_exit.


    cl_http_server=>get_location( IMPORTING host = lv_host ).
    APPEND lv_host TO lt_list.

    APPEND 'localhost' TO lt_list.

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_local_host( CHANGING ct_hosts = lt_list ).

    FIND REGEX 'https?://([^/^:]*)' IN iv_url SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY table_line = lv_host TRANSPORTING NO FIELDS.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP implementation

*>>>>>>> ZCL_ABAPGIT_HTTP_CLIENT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_http_client=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_http_client=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_HTTP_CLIENT implementation.
*"* method's implementations
*include methods.
  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.

    mi_client->response->get_status( IMPORTING code = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN. " Success, OK
      WHEN 302.
        Lcx_abapgit_exception=>raise( 'Resource access temporarily redirected (HTTP 302). Check the URL' ).
      WHEN 401.
        Lcx_abapgit_exception=>raise( 'Unauthorized access to resource (HTTP 401). Check your credentials' ).
      WHEN 403.
        Lcx_abapgit_exception=>raise( 'Access to resource forbidden (HTTP 403)' ).
      WHEN 404.
        Lcx_abapgit_exception=>raise( 'Resource not found (HTTP 404). Check the URL' ).
      WHEN 407.
        Lcx_abapgit_exception=>raise( 'Proxy authentication required (HTTP 407). Check your credentials' ).
      WHEN 408.
        Lcx_abapgit_exception=>raise( 'Request timeout (HTTP 408)' ).
      WHEN 415.
        Lcx_abapgit_exception=>raise( 'Unsupported media type (HTTP 415)' ).
      WHEN 422.
        Lcx_abapgit_exception=>raise( 'Unprocessable entity (HTTP 422). Check, if URL has to end with ".git"' ).
      WHEN OTHERS.
        lv_text = mi_client->response->get_cdata( ).
        Lcx_abapgit_exception=>raise( |(HTTP { lv_code }) { lv_text }| ).
    ENDCASE.

  ENDMETHOD.
  METHOD check_smart_response.

    DATA: lv_content_type TYPE string.
    DATA: lv_data         TYPE string.

    IF iv_expected_content_type IS NOT INITIAL.
      lv_content_type = mi_client->response->get_content_type( ).
      IF lv_content_type <> iv_expected_content_type.
        Lcx_abapgit_exception=>raise( 'Wrong Content-Type sent by server - no fallback to the dumb protocol!' ).
      ENDIF.
    ENDIF.

    IF iv_content_regex IS NOT INITIAL.
      lv_data = mi_client->response->get_cdata( ).
      FIND REGEX iv_content_regex IN lv_data.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Wrong Content sent by server' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD close.
    mi_client->close( ).
  ENDMETHOD.
  METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.
  METHOD get_cdata.
    rv_value = mi_client->response->get_cdata( ).
  ENDMETHOD.
  METHOD send_receive.

    DATA: lv_text    TYPE string,
          lv_code    TYPE i,
          lv_message TYPE string.

    mi_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc = 0.
      mi_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      " in case of HTTP_COMMUNICATION_FAILURE
      " make sure:
      " a) SSL is setup properly in STRUST
      " b) no firewalls
      " check trace file in transaction SMICM

      mi_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      lv_text = |HTTP error { lv_code } occured: { lv_message }|.

      Lcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

  ENDMETHOD.
  METHOD send_receive_close.

* do not use set_cdata as it modifies the Content-Type header field
    mi_client->request->set_data( iv_data ).
    send_receive( ).
    check_http_200( ).
    rv_data = mi_client->response->get_data( ).
    mi_client->close( ).

  ENDMETHOD.
  METHOD set_digest.
    mo_digest = io_digest.
  ENDMETHOD.
  METHOD set_headers.

    DATA: lv_value TYPE string.


    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = Lcl_abapgit_url=>path_name( iv_url ) &&
      '/git-' &&
      iv_service &&
      '-pack'.
    mi_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.
    mi_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.
    mi_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).

    IF mo_digest IS BOUND.
      mo_digest->run( mi_client ).
    ENDIF.

  ENDMETHOD.
  METHOD set_header.
    mi_client->request->set_header_field(
      name  = iv_key
      value = iv_value ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_HTTP_CLIENT implementation

*>>>>>>> ZCL_ABAPGIT_JSON_HANDLER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_json_handler======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_json_handler======ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW3ISFY DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES Lif_abapgit_ajson_filter.
    TYPES:
      BEGIN OF ty_path_value_pair,
        path  TYPE string,
        value TYPE string,
      END OF ty_path_value_pair,
      ty_skip_paths TYPE STANDARD TABLE OF ty_path_value_pair WITH KEY path.

    METHODS constructor
      IMPORTING iv_skip_paths TYPE ty_skip_paths OPTIONAL
      RAISING   Lcx_abapgit_ajson_error.
  PRIVATE SECTION.
    DATA mt_skip_paths TYPE ty_skip_paths.
ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYB3KW3ISFY IMPLEMENTATION.

  METHOD Lif_abapgit_ajson_filter~keep_node.

    DATA lv_path TYPE string.

    lv_path = is_node-path && is_node-name.

    READ TABLE mt_skip_paths WITH KEY path = lv_path value = is_node-value TRANSPORTING NO FIELDS.
    IF boolc( sy-subrc = 0 ) = abap_true
      AND iv_visit = Lif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ELSE.
      READ TABLE mt_skip_paths WITH KEY path = lv_path TRANSPORTING NO FIELDS.
      IF boolc( sy-subrc = 0 ) = abap_true
        AND iv_visit = Lif_abapgit_ajson_filter=>visit_type-value.
        rv_keep = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF is_node-type = 'bool' AND is_node-value = 'false' AND iv_visit = Lif_abapgit_ajson_filter=>visit_type-value.
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    " AFF: if INTEGER type is initial (0) then is will be skipped
    "      However, if type is $required, it should be serialized.
    IF NOT ( ( iv_visit = Lif_abapgit_ajson_filter=>visit_type-value AND is_node-value IS NOT INITIAL ) OR
         ( iv_visit <> Lif_abapgit_ajson_filter=>visit_type-value AND is_node-children > 0 ) ).
      rv_keep = abap_false.
      RETURN.
    ENDIF.

    rv_keep = abap_true.

  ENDMETHOD.

  METHOD constructor.
    " extract annotations and build table for values to be skipped ( path/name | value )
    DATA lo_abap_language_pair TYPE ty_path_value_pair.
    lo_abap_language_pair-path = `/header/abapLanguageVersion`.
    lo_abap_language_pair-value = 'standard'.

    APPEND lo_abap_language_pair TO mt_skip_paths.

    IF iv_skip_paths IS NOT INITIAL.
      APPEND LINES OF iv_skip_paths TO mt_skip_paths.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_JSON_HANDLER implementation.
*"* method's implementations
*include methods.
  METHOD deserialize.
    DATA lv_json    TYPE string.
    DATA lo_ajson   TYPE REF TO Lif_abapgit_ajson.

    CLEAR ev_data.

    lv_json = Lcl_abapgit_convert=>xstring_to_string_utf8( iv_content ).

    lo_ajson = Lcl_abapgit_ajson=>parse( lv_json
      )->map( Lcl_abapgit_ajson_mapping=>create_to_snake_case( ) ).

    map2abap_original_language( CHANGING co_ajson = lo_ajson ).
    set_defaults( EXPORTING it_defaults = iv_defaults
                  CHANGING  co_ajson    = lo_ajson ).
    map2abap_abap_language_version( CHANGING co_ajson = lo_ajson ).
    map2abap_custom_enum( EXPORTING it_enum_mappings = iv_enum_mappings
                          CHANGING co_ajson          = lo_ajson ).

    lo_ajson->to_abap( IMPORTING ev_container = ev_data ).

  ENDMETHOD.
  METHOD map2abap_abap_language_version.
    DATA:
      lv_enum_abap TYPE string,
      lv_enum_json TYPE string.


    lv_enum_json = co_ajson->get_string( '/header/abap_language_version' ).
    IF lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
      lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
    ELSEIF lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
      lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
    ELSEIF lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
      lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
    ENDIF.

    co_ajson->set_string( iv_path = '/header/abap_language_version'
                          iv_val  = lv_enum_abap ).
  ENDMETHOD.
  METHOD map2abap_custom_enum.
    DATA:
      lv_enum_json    TYPE string,
      ls_enum_mapping TYPE ty_enum_mapping,
      ls_mapping      TYPE ty_json_abap_mapping.


    LOOP AT it_enum_mappings INTO ls_enum_mapping.
      lv_enum_json = co_ajson->get_string( ls_enum_mapping-path ).
      READ TABLE ls_enum_mapping-mappings WITH KEY json = lv_enum_json INTO ls_mapping.
      IF sy-subrc = 0.
        co_ajson->set_string( iv_path = ls_enum_mapping-path
                              iv_val  = ls_mapping-abap ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD map2abap_original_language.
    DATA:
      lv_iso_language      TYPE laiso,
      lv_original_language TYPE sy-langu.


    lv_iso_language = co_ajson->get_string( '/header/original_language' ).

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING
        input  = lv_iso_language
      IMPORTING
        output = lv_original_language.

    co_ajson->set_string( iv_path = '/header/original_language'
                          iv_val  = lv_original_language ).
  ENDMETHOD.
  METHOD map2json_abap_language_version.
    DATA:
      lv_enum_abap TYPE string,
      lv_enum_json TYPE string.


    lv_enum_abap = co_ajson->get_string( '/header/abapLanguageVersion' ).
    IF lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
      OR lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-standard.
      lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard.
    ELSEIF lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development.
      lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.
    ELSEIF lv_enum_abap = Lif_abapgit_aff_types_v1=>co_abap_language_version-key_user.
      lv_enum_json = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user.
    ENDIF.

    co_ajson->set_string( iv_path = '/header/abapLanguageVersion'
                          iv_val  = lv_enum_json ).
  ENDMETHOD.
  METHOD map2json_custom_enum.
    DATA:
      lv_enum_abap    TYPE string,
      ls_enum_mapping TYPE ty_enum_mapping,
      ls_mapping      TYPE ty_json_abap_mapping.


    LOOP AT it_enum_mappings INTO ls_enum_mapping.
      lv_enum_abap = co_ajson->get_string( ls_enum_mapping-path ).
      READ TABLE ls_enum_mapping-mappings WITH KEY abap = lv_enum_abap INTO ls_mapping.
      IF sy-subrc = 0.
        co_ajson->set_string( iv_path = ls_enum_mapping-path
                              iv_val  = ls_mapping-json ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD map2json_original_language.
    DATA:
      lv_iso_language      TYPE laiso,
      lv_original_language TYPE sy-langu.


    lv_original_language = co_ajson->get_string( '/header/originalLanguage' ).

    lv_iso_language = Lcl_abapgit_convert=>conversion_exit_isola_output( lv_original_language ).

    TRANSLATE lv_iso_language TO LOWER CASE.
    co_ajson->set_string( iv_path = '/header/originalLanguage'
                          iv_val  = lv_iso_language ).
  ENDMETHOD.
  METHOD serialize.
    DATA: lt_st_source TYPE abap_trans_srcbind_tab,
          lv_json      TYPE string,
          lo_ajson     TYPE REF TO Lif_abapgit_ajson,
          lo_filter    TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW3ISFY.

    FIELD-SYMBOLS: <lg_source> LIKE LINE OF lt_st_source.

    APPEND INITIAL LINE TO lt_st_source ASSIGNING <lg_source>.
    GET REFERENCE OF iv_data INTO <lg_source>-value.

    lo_ajson = Lcl_abapgit_ajson=>new( iv_keep_item_order = abap_true
      )->set( iv_path = '/'
              iv_val  = iv_data
      )->map( Lcl_abapgit_ajson_mapping=>create_to_camel_case( ) ).

    map2json_original_language( CHANGING co_ajson = lo_ajson ).
    map2json_abap_language_version( CHANGING co_ajson = lo_ajson ).
    map2json_custom_enum( EXPORTING it_enum_mappings = iv_enum_mappings
                          CHANGING co_ajson          = lo_ajson ).

    CREATE OBJECT lo_filter EXPORTING iv_skip_paths = iv_skip_paths.

    " files end with an empty line (EOF)
    lv_json = lo_ajson->clone( )->filter( lo_filter )->stringify( 2 ) && cl_abap_char_utilities=>newline.

    rv_result = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_json ).
  ENDMETHOD.
  METHOD set_defaults.
    DATA:
      lv_enum_json TYPE string,
      ls_default   TYPE ty_path_value_pair.


    LOOP AT it_defaults INTO ls_default.
      lv_enum_json = co_ajson->get_string( ls_default-path ).
      IF lv_enum_json = ``.
        co_ajson->set_string( iv_path = ls_default-path
                              iv_val  = ls_default-value ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_JSON_HANDLER implementation

*>>>>>>> ZCL_ABAPGIT_LXE_TEXTS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_lxe_texts=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_lxe_texts=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_lxe_texts=========ccau.

*CLASS zcl_abapgit_lxe_texts DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW3PSFY.


class LCL_ABAPGIT_LXE_TEXTS implementation.
*"* method's implementations
*include methods.
  METHOD check_langs_versus_installed.

    DATA lt_installed_hash TYPE HASHED TABLE OF laiso WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS <lv_lang> LIKE LINE OF it_languages.

    CLEAR: et_intersection, et_missfits.
    lt_installed_hash = it_installed.

    LOOP AT it_languages ASSIGNING <lv_lang>.
      READ TABLE lt_installed_hash WITH KEY table_line = <lv_lang> TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        APPEND <lv_lang> TO et_intersection.
      ELSE.
        APPEND <lv_lang> TO et_missfits.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD convert_lang_string_to_table.

    DATA:
      lt_langs_str          TYPE string_table,
      lv_laiso              TYPE laiso,
      lv_skip_main_lang_iso TYPE laiso.

    FIELD-SYMBOLS:
      <lv_str>  LIKE LINE OF lt_langs_str.

    " Keep * as indicator for 'all installed languages'
    IF iv_langs = '*'.
      APPEND iv_langs TO rt_languages.
      RETURN.
    ENDIF.

    " Convert string of 2-letter ISO languages into table of sy-langu codes
    SPLIT iv_langs AT ',' INTO TABLE lt_langs_str.

    LOOP AT lt_langs_str ASSIGNING <lv_str>.
      lv_laiso = condense( to_upper( <lv_str> ) ).
      APPEND lv_laiso TO rt_languages.
    ENDLOOP.

    IF iv_skip_main_language IS NOT INITIAL.
      lv_skip_main_lang_iso = langu_to_laiso_safe( iv_skip_main_language ).
      DELETE rt_languages WHERE table_line = lv_skip_main_lang_iso.
    ENDIF.

    SORT rt_languages.
    DELETE ADJACENT DUPLICATES FROM rt_languages.

  ENDMETHOD.
  METHOD convert_table_to_lang_string.

    DATA:
      lt_langs_str TYPE string_table.

    FIELD-SYMBOLS:
      <lv_lang> LIKE LINE OF it_languages,
      <lv_str>  TYPE string.

    " Convert table of sy-langu codes into string of 2-letter ISO languages
    LOOP AT it_languages ASSIGNING <lv_lang>.
      " Keep * as indicator for 'all installed languages'
      IF <lv_lang> = '*'.
        CLEAR lt_langs_str.
        APPEND '*' TO lt_langs_str.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_langs_str ASSIGNING <lv_str>.
      <lv_str> = <lv_lang>.
    ENDLOOP.

    CONCATENATE LINES OF lt_langs_str INTO rv_langs SEPARATED BY ','.

  ENDMETHOD.
  METHOD detect_unsupported_languages.

    check_langs_versus_installed(
      EXPORTING
        it_languages = it_languages
        it_installed = get_installed_languages( )
      IMPORTING
        et_missfits = rt_unsupported_languages ).

  ENDMETHOD.
  METHOD get_installed_languages.

    DATA:
      lv_index               TYPE i,
      lv_langu               TYPE sy-langu,
      lv_laiso               TYPE laiso,
      lv_installed_languages TYPE string,
      lt_language_filter     TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF gt_installed_languages_cache IS INITIAL.
      CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
        IMPORTING
          languages       = lv_installed_languages
        EXCEPTIONS
          sapgparam_error = 1                " Error requesting profile parameter
          OTHERS          = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Fail to get system SYSTEM_INSTALLED_LANGUAGES' ).
      ENDIF.

      lt_language_filter = Lcl_abapgit_factory=>get_environment( )->get_system_language_filter( ).

      DO strlen( lv_installed_languages ) TIMES.
        lv_index = sy-index - 1.
        lv_langu = lv_installed_languages+lv_index(1).

        IF lv_langu NOT IN lt_language_filter.
          CONTINUE.
        ENDIF.

        lv_laiso = langu_to_laiso_safe( lv_langu ).
        APPEND lv_laiso TO gt_installed_languages_cache.
      ENDDO.
    ENDIF.

    rt_languages = gt_installed_languages_cache.

  ENDMETHOD.
  METHOD get_lang_iso4.

    DATA lv_lang_iso639 TYPE laiso.
    DATA lv_country     TYPE land1.
    DATA lv_class       TYPE string.

    lv_class = 'CL_I18N_LANGUAGES'.

" cannot find a way to do this in Steampunk, so dynamic for now,
    CALL METHOD (lv_class)=>sap2_to_iso639_1
      EXPORTING
        im_lang_sap2   = iv_src
      IMPORTING
        ex_lang_iso639 = lv_lang_iso639
        ex_country     = lv_country
      EXCEPTIONS
        no_assignment  = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Failed to convert [{ iv_src }] lang to iso639| ).
    ENDIF.

    CONCATENATE lv_lang_iso639 lv_country INTO rv_iso4.

  ENDMETHOD.
  METHOD get_lxe_object_list.

    DATA lv_object_name TYPE trobj_name.

    lv_object_name = iv_object_name.

    CALL FUNCTION 'LXE_OBJ_EXPAND_TRANSPORT_OBJ'
      EXPORTING
        pgmid           = 'R3TR'
        object          = iv_object_type
        obj_name        = lv_object_name
      TABLES
        ex_colob        = rt_obj_list
      EXCEPTIONS
        unknown_object  = 1
        unknown_ta_type = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN. " Ignore error and return empty list
    ENDIF.

  ENDMETHOD.
  METHOD get_translation_languages.

    " Returns a list of translation languages for serialization
    " If the setting is initial, no translations shall be serialized
    " If the setting is `*`, all all installed system languages shall be serialized
    " Else, the setting shall contain all languages to be serialized

    DATA lv_main_lang_laiso TYPE laiso.

    IF it_i18n_languages IS NOT INITIAL.
      READ TABLE it_i18n_languages TRANSPORTING NO FIELDS WITH KEY table_line = '*'.
      IF sy-subrc = 0.
        rt_languages = get_installed_languages( ).
      ELSE.
        check_langs_versus_installed(
          EXPORTING
            it_languages = it_i18n_languages
            it_installed = get_installed_languages( )
          IMPORTING
            et_intersection = rt_languages ).
      ENDIF.
    ENDIF.

    " Remove main language from translation languages
    lv_main_lang_laiso = langu_to_laiso_safe( iv_main_language ).
    DELETE rt_languages WHERE table_line = lv_main_lang_laiso.

  ENDMETHOD.
  METHOD langu_to_laiso_safe.

    Lcl_abapgit_convert=>language_sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_langu
      RECEIVING
        re_lang_sap2  = rv_laiso
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Could not convert lang [{ iv_langu }] to ISO| ).
    ENDIF.

  ENDMETHOD.
  METHOD read_lxe_object_text_pair.

    DATA:
      lv_error TYPE lxestring.

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
            read_only = iv_read_only
          IMPORTING
            err_msg   = lv_error  " doesn't exist in NW <= 750
          TABLES
            lt_pcx_s1 = rt_text_pairs_tmp.
        IF lv_error IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( lv_error ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
            read_only = iv_read_only
          TABLES
            lt_pcx_s1 = rt_text_pairs_tmp.

    ENDTRY.

  ENDMETHOD.
  METHOD write_lxe_object_text_pair.

    DATA:
      lv_error TYPE lxestring.

    TRY.
        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
          IMPORTING
            err_msg   = lv_error  " doesn't exist in NW <= 750
          TABLES
            lt_pcx_s1 = it_pcx_s1.
        IF lv_error IS NOT INITIAL.
          Lcx_abapgit_exception=>raise( lv_error ).
        ENDIF.

      CATCH cx_sy_dyn_call_param_not_found.

        CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_WRITE'
          EXPORTING
            s_lang    = iv_s_lang
            t_lang    = iv_t_lang
            custmnr   = iv_custmnr
            objtype   = iv_objtype
            objname   = iv_objname
          TABLES
            lt_pcx_s1 = it_pcx_s1.

    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_lxe_texts~deserialize.

    IF is_object_supported( iv_object_type ) = abap_false.
      RETURN.
    ENDIF.

    mo_i18n_params = io_i18n_params.
    mi_xml_in      = ii_xml.
    mo_files       = io_files.

    " MAYBE TODO: see comment in serialize

    IF 1 = 1.
      deserialize_from_po(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ELSE.
      deserialize_xml(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_lxe_texts~serialize.

    IF is_object_supported( iv_object_type ) = abap_false.
      RETURN.
    ENDIF.

    mo_i18n_params = io_i18n_params.
    mi_xml_out     = ii_xml.
    mo_files       = io_files.

    " MAYBE TODO
    " if other formats are needed, including the old in-XML approach
    " here is the place to implement it. Supposed architecture:
    " I18N_PARAMS should contain an option which format to use
    " The option should be originally maintained in dot_abapgit structures (e.g. `translation_storage_format`)
    " Consequently it comes here
    " The serialize method can read it and call a corresponding submethod,
    " e.g. serialize_xml or serialize_as_po or ...
    " both ii_xml and io_files are accessible intentionally to enable both XML based or file based formats
    " access to json can be easily added too,
    " or maybe (maybe) some kind of zif_ag_object_ctl with all DAO instead

    IF 1 = 1.
      serialize_as_po(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ELSE.
      serialize_xml(
        iv_object_type = iv_object_type
        iv_object_name = iv_object_name ).
    ENDIF.

  ENDMETHOD.
  METHOD class_constructor.

    APPEND 'CLAS' TO gt_supported_obj_types.
    APPEND 'DOMA' TO gt_supported_obj_types.
    APPEND 'DTEL' TO gt_supported_obj_types.
    APPEND 'FUGR' TO gt_supported_obj_types.
    APPEND 'MSAG' TO gt_supported_obj_types.
    APPEND 'PARA' TO gt_supported_obj_types.
    APPEND 'PROG' TO gt_supported_obj_types.
    APPEND 'SHI3' TO gt_supported_obj_types.
    APPEND 'TABL' TO gt_supported_obj_types.
    APPEND 'TRAN' TO gt_supported_obj_types.
    APPEND 'VIEW' TO gt_supported_obj_types.

  ENDMETHOD.
  METHOD deserialize_from_po.

    DATA lv_lang LIKE LINE OF mo_i18n_params->ms_params-translation_languages.
    DATA lt_po_files TYPE Lif_abapgit_i18n_file=>ty_table_of.
    DATA li_po LIKE LINE OF lt_po_files.
    DATA lt_text_pairs_tmp TYPE ty_lxe_translation-text_pairs.
    DATA lt_obj_list TYPE lxe_tt_colob.
    DATA lv_main_lang TYPE lxeisolang.
    DATA lv_target_lang TYPE lxeisolang.

    FIELD-SYMBOLS <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
      iv_object_name = iv_object_name
      iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    lt_po_files  = mo_files->read_i18n_files( ).
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( mo_i18n_params->ms_params-main_language ) ).

    LOOP AT mo_i18n_params->ms_params-translation_languages INTO lv_lang.
      lv_target_lang = get_lang_iso4( lv_lang ).

      LOOP AT lt_po_files INTO li_po.
        IF li_po->lang( ) = to_lower( lv_lang ). " Not quite efficient but the list is presumably very short
          EXIT.
        ELSE.
          CLEAR li_po.
        ENDIF.
      ENDLOOP.

      CHECK li_po IS BOUND. " Ignore missing files, missing translation is not a crime

      LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.

        lt_text_pairs_tmp = read_lxe_object_text_pair(
          iv_s_lang    = lv_main_lang
          iv_t_lang    = lv_target_lang
          iv_custmnr   = <lv_lxe_object>-custmnr
          iv_objtype   = <lv_lxe_object>-objtype
          iv_objname   = <lv_lxe_object>-objname
          iv_read_only = abap_false ).

        li_po->translate( CHANGING ct_text_pairs = lt_text_pairs_tmp ).
        " TODO maybe optimize, check if values have changed

        write_lxe_object_text_pair(
          iv_s_lang  = lv_main_lang
          iv_t_lang  = lv_target_lang
          iv_custmnr = <lv_lxe_object>-custmnr
          iv_objtype = <lv_lxe_object>-objtype
          iv_objname = <lv_lxe_object>-objname
          it_pcx_s1  = lt_text_pairs_tmp ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_xml.

    DATA:
      lt_lxe_texts      TYPE ty_lxe_translations,
      ls_lxe_item       LIKE LINE OF lt_lxe_texts,
      lt_text_pairs_tmp LIKE ls_lxe_item-text_pairs.

    mi_xml_in->read(
      EXPORTING iv_name = iv_lxe_text_name
      CHANGING  cg_data = lt_lxe_texts ).

    LOOP AT lt_lxe_texts INTO ls_lxe_item.
      " Call Read first for buffer prefill

      lt_text_pairs_tmp = read_lxe_object_text_pair(
        iv_s_lang    = ls_lxe_item-source_lang
        iv_t_lang    = ls_lxe_item-target_lang
        iv_custmnr   = ls_lxe_item-custmnr
        iv_objtype   = ls_lxe_item-objtype
        iv_objname   = ls_lxe_item-objname
        iv_read_only = abap_false ).

      "Call actual Write FM
      write_lxe_object_text_pair(
        iv_s_lang  = ls_lxe_item-source_lang
        iv_t_lang  = ls_lxe_item-target_lang
        iv_custmnr = ls_lxe_item-custmnr
        iv_objtype = ls_lxe_item-objtype
        iv_objname = ls_lxe_item-objname
        it_pcx_s1  = ls_lxe_item-text_pairs ).

    ENDLOOP.

  ENDMETHOD.
  METHOD iso4_to_iso2.
    rv_laiso = iv_lxe_lang+0(2).
  ENDMETHOD.
  METHOD is_object_supported.
    READ TABLE gt_supported_obj_types TRANSPORTING NO FIELDS WITH KEY table_line = iv_object_type.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD read_text_items.

    DATA:
      lt_obj_list      TYPE lxe_tt_colob,
      lv_main_lang     TYPE lxeisolang,
      ls_lxe_text_item LIKE LINE OF rt_text_items.

    FIELD-SYMBOLS:
      <lv_language>   LIKE LINE OF mo_i18n_params->ms_params-translation_languages,
      <lv_lxe_object> LIKE LINE OF lt_obj_list.

    lt_obj_list = get_lxe_object_list(
      iv_object_name = iv_object_name
      iv_object_type = iv_object_type ).

    IF lt_obj_list IS INITIAL.
      RETURN.
    ENDIF.

    " Get list of languages that need to be serialized (already resolves * and installed languages)
    lv_main_lang = get_lang_iso4( langu_to_laiso_safe( mo_i18n_params->ms_params-main_language ) ).

    LOOP AT lt_obj_list ASSIGNING <lv_lxe_object>.
      CLEAR ls_lxe_text_item.
      ls_lxe_text_item-custmnr = <lv_lxe_object>-custmnr.
      ls_lxe_text_item-objtype = <lv_lxe_object>-objtype.
      ls_lxe_text_item-objname = <lv_lxe_object>-objname.

      LOOP AT mo_i18n_params->ms_params-translation_languages ASSIGNING <lv_language>.
        ls_lxe_text_item-source_lang = lv_main_lang.
        ls_lxe_text_item-target_lang = get_lang_iso4( <lv_language> ).
        IF ls_lxe_text_item-source_lang = ls_lxe_text_item-target_lang.
          CONTINUE. " if source = target -> skip
        ENDIF.

        ls_lxe_text_item-text_pairs = read_lxe_object_text_pair(
          iv_s_lang    = ls_lxe_text_item-source_lang
          iv_t_lang    = ls_lxe_text_item-target_lang
          iv_custmnr   = ls_lxe_text_item-custmnr
          iv_objtype   = ls_lxe_text_item-objtype
          iv_objname   = ls_lxe_text_item-objname ).

        IF ls_lxe_text_item-text_pairs IS NOT INITIAL.
          APPEND ls_lxe_text_item TO rt_text_items.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_as_po.

    DATA lt_lxe_texts TYPE ty_lxe_translations.
    DATA lo_po_file TYPE REF TO Lcl_abapgit_po_file.
    DATA lv_lang LIKE LINE OF mo_i18n_params->ms_params-translation_languages.
    FIELD-SYMBOLS <ls_translation> LIKE LINE OF lt_lxe_texts.

    lt_lxe_texts = read_text_items(
      iv_object_name   = iv_object_name
      iv_object_type   = iv_object_type ).

    LOOP AT mo_i18n_params->ms_params-translation_languages INTO lv_lang.
      lv_lang = to_lower( lv_lang ).
      CREATE OBJECT lo_po_file
        EXPORTING
          iv_lang = lv_lang.
      LOOP AT lt_lxe_texts ASSIGNING <ls_translation>.
        IF iso4_to_iso2( <ls_translation>-target_lang ) = lv_lang.
          lo_po_file->push_text_pairs(
            iv_objtype    = <ls_translation>-objtype
            iv_objname    = <ls_translation>-objname
            it_text_pairs = <ls_translation>-text_pairs ).
        ENDIF.
      ENDLOOP.
      mo_files->add_i18n_file( lo_po_file ).
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_xml.

    DATA lt_lxe_texts TYPE ty_lxe_translations.

    lt_lxe_texts = read_text_items(
      iv_object_name   = iv_object_name
      iv_object_type   = iv_object_type ).

    IF lines( lt_lxe_texts ) > 0.
      mi_xml_out->add(
        iv_name = iv_lxe_text_name
        ig_data = lt_lxe_texts ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_LXE_TEXTS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_CHECK <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_check=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_check=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_check=====ccau.

*CLASS zcl_abapgit_objects_check DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW37SFY.


class LCL_ABAPGIT_OBJECTS_CHECK implementation.
*"* method's implementations
*include methods.
  METHOD checks_adjust.

    warning_overwrite_adjust(
      EXPORTING
        it_overwrite = is_checks-overwrite
      CHANGING
        ct_results   = ct_results ).

    warning_package_adjust(
      EXPORTING
        io_repo      = io_repo
        it_overwrite = is_checks-warning_package
      CHANGING
        ct_results   = ct_results ).

  ENDMETHOD.
  METHOD check_multiple_files.

    DATA:
      lv_msg      TYPE string,
      lv_lstate   TYPE c LENGTH 2,
      lv_rstate   TYPE c LENGTH 2,
      lt_res_sort LIKE it_results,
      ls_result   LIKE LINE OF it_results.

    FIELD-SYMBOLS <ls_result> LIKE LINE OF it_results.

    lt_res_sort = it_results.
    SORT lt_res_sort BY filename ASCENDING.

    " Prevent pulling if there is more than one file with the same name
    LOOP AT lt_res_sort ASSIGNING <ls_result>
      WHERE obj_type <> 'DEVC' AND packmove = abap_false AND filename IS NOT INITIAL.
      " Changing package and object at the same time is ok (state: Add + Delete)
      CONCATENATE <ls_result>-lstate ls_result-lstate INTO lv_lstate RESPECTING BLANKS.
      CONCATENATE <ls_result>-rstate ls_result-rstate INTO lv_rstate RESPECTING BLANKS.
      IF <ls_result>-filename = ls_result-filename AND
        lv_lstate <> 'AD' AND lv_lstate <> 'DA' AND lv_rstate <> 'AD' AND lv_rstate <> 'DA'.
        lv_msg = |Pull not possible since there are multiple files with same filename, { <ls_result>-filename }.|
          && | Keep one of the files and delete the other in the repository.|.
        Lcx_abapgit_exception=>raise( lv_msg ).
      ENDIF.
      MOVE-CORRESPONDING <ls_result> TO ls_result.
    ENDLOOP.

  ENDMETHOD.
  METHOD class_constructor.

    gi_exit = Lcl_abapgit_exit=>get_instance( ).

  ENDMETHOD.
  METHOD deserialize_checks.

    DATA: lt_results TYPE Lif_abapgit_definitions=>ty_results_tt,
          li_package TYPE REF TO Lif_abapgit_sap_package.

    " get unfiltered status to evaluate properly which warnings are required
    lt_results = Lcl_abapgit_repo_status=>calculate( io_repo ).

    check_multiple_files( lt_results ).

    rs_checks-overwrite = warning_overwrite_find( lt_results ).

    rs_checks-warning_package = warning_package_find(
      io_repo    = io_repo
      it_results = lt_results ).

    IF lines( lt_results ) > 0.
      li_package = Lcl_abapgit_factory=>get_sap_package( io_repo->get_package( ) ).
      rs_checks-transport-required = li_package->are_changes_recorded_in_tr_req( ).
      IF NOT rs_checks-transport-required IS INITIAL.
        rs_checks-transport-type = li_package->get_transport_type( ).
        rs_checks-transport-transport = determine_transport_request(
                                            io_repo           = io_repo
                                            iv_transport_type = rs_checks-transport-type ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD determine_transport_request.

    " Use transport from repo settings if maintained, or determine via user exit.
    " If transport keeps empty here, it'll requested later via popup.
    rv_transport_request = io_repo->get_local_settings( )-transport_request.

    gi_exit->determine_transport_request(
      EXPORTING
        io_repo              = io_repo
        iv_transport_type    = iv_transport_type
      CHANGING
        cv_transport_request = rv_transport_request ).

  ENDMETHOD.
  METHOD warning_overwrite_adjust.

    DATA: lt_overwrite LIKE it_overwrite,
          ls_overwrite LIKE LINE OF lt_overwrite.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF lt_overwrite.


* make sure to get the current status, as something might have changed in the meanwhile
    lt_overwrite = warning_overwrite_find( ct_results ).

    LOOP AT lt_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite INTO ls_overwrite
                              WITH TABLE KEY object_type_and_name
                              COMPONENTS obj_type = <ls_overwrite>-obj_type
                                         obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        Lcx_abapgit_exception=>raise( |Overwrite { <ls_overwrite>-obj_type } {
          <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = Lif_abapgit_definitions=>c_no.
        DELETE ct_results WHERE
          obj_type = <ls_overwrite>-obj_type AND
          obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD warning_overwrite_find.

    DATA:
      ls_item    TYPE Lif_abapgit_definitions=>ty_item,
      lv_status  TYPE c LENGTH 2,
      lt_changes TYPE STANDARD TABLE OF Lif_abapgit_definitions=>ty_overwrite WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_result>  LIKE LINE OF it_results,
      <ls_changes> LIKE LINE OF lt_changes.

    " collect all actions for object that have been changed
    LOOP AT it_results ASSIGNING <ls_result> WHERE NOT obj_type IS INITIAL.

      APPEND INITIAL LINE TO lt_changes ASSIGNING <ls_changes>.
      MOVE-CORRESPONDING <ls_result> TO <ls_changes>.
      <ls_changes>-devclass = <ls_result>-package.
      MOVE-CORRESPONDING <ls_changes> TO ls_item.

      IF <ls_result>-packmove = abap_true.
        <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-packmove.
        <ls_changes>-icon   = icon_package_standard.
        <ls_changes>-text   = 'Change package assignment'.
      ELSEIF Lcl_abapgit_objects=>is_supported( ls_item ) = abap_false
        AND ls_item-obj_type <> Lif_abapgit_data_config=>c_data_type-tabu.
        <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-no_support.
        <ls_changes>-icon   = icon_no_status.
        <ls_changes>-text   = 'Object type not supported'.
      ELSE.
        CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO lv_status RESPECTING BLANKS.
        <ls_changes>-state = lv_status.
        REPLACE ALL OCCURRENCES OF ` ` IN <ls_changes>-state WITH '_'.

        CASE lv_status.
          WHEN '  '. " no changes
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-none.
          WHEN ' A' OR 'D ' OR 'DM'. " added remotely or deleted locally
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-add.
            <ls_changes>-icon   = icon_create.
            <ls_changes>-text   = 'Add local object'.
          WHEN 'A ' OR ' D' OR 'MD'. " added locally or deleted remotely
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-delete.
            <ls_changes>-icon   = icon_delete.
            <ls_changes>-text   = 'Delete local object'.
          WHEN 'M ' OR 'MM'. " modified locally
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-overwrite.
            <ls_changes>-icon   = icon_change.
            <ls_changes>-text   = 'Overwrite local object'.
          WHEN ' M'. " modified only remotely
            <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-update.
            <ls_changes>-icon   = icon_change.
            <ls_changes>-text   = 'Update local object'.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      ENDIF.

    ENDLOOP.

    " Remove duplicate actions
    SORT lt_changes.
    DELETE ADJACENT DUPLICATES FROM lt_changes.

    " Check if deletions are for complete object or just a part
    LOOP AT lt_changes ASSIGNING <ls_changes> WHERE action = Lif_abapgit_objects=>c_deserialize_action-delete.

      LOOP AT lt_changes TRANSPORTING NO FIELDS
        WHERE obj_type = <ls_changes>-obj_type AND obj_name = <ls_changes>-obj_name
          AND action <> Lif_abapgit_objects=>c_deserialize_action-delete.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        " There's some other action, so object will be recreated after deletion
        <ls_changes>-action = Lif_abapgit_objects=>c_deserialize_action-delete_add.
        <ls_changes>-icon   = icon_adopt.
        <ls_changes>-text   = 'Delete and recreate local object'.
      ENDIF.

    ENDLOOP.

    DELETE lt_changes WHERE action = Lif_abapgit_objects=>c_deserialize_action-none.

    " If there are multiple changes in an object, keep highest priority action
    SORT lt_changes BY obj_type obj_name action DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_changes COMPARING obj_type obj_name.

    rt_overwrite = lt_changes.

  ENDMETHOD.
  METHOD warning_package_adjust.

    DATA: lt_overwrite LIKE it_overwrite,
          ls_overwrite LIKE LINE OF lt_overwrite.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF lt_overwrite.


* make sure to get the current status, as something might have changed in the meanwhile
    lt_overwrite = warning_package_find(
      it_results   = ct_results
      io_repo      = io_repo ).

    LOOP AT lt_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE it_overwrite INTO ls_overwrite
                              WITH TABLE KEY object_type_and_name
                              COMPONENTS obj_type = <ls_overwrite>-obj_type
                                         obj_name = <ls_overwrite>-obj_name.
      IF sy-subrc <> 0 OR ls_overwrite-decision IS INITIAL.
        Lcx_abapgit_exception=>raise( |Overwrite of package { <ls_overwrite>-obj_type } {
          <ls_overwrite>-obj_name } undecided| ).
      ENDIF.

      IF ls_overwrite-decision = Lif_abapgit_definitions=>c_no.
        DELETE ct_results WHERE
          obj_type = <ls_overwrite>-obj_type AND
          obj_name = <ls_overwrite>-obj_name.
        ASSERT sy-subrc = 0.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD warning_package_find.

    DATA: lv_package          TYPE devclass,
          lt_overwrite_unique TYPE HASHED TABLE OF Lif_abapgit_definitions=>ty_overwrite
                                  WITH UNIQUE KEY obj_type obj_name devclass,
          ls_overwrite        LIKE LINE OF rt_overwrite,
          ls_tadir            TYPE Lif_abapgit_definitions=>ty_tadir.

    DATA: lo_folder_logic TYPE REF TO Lcl_abapgit_folder_logic.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).
    LOOP AT it_results ASSIGNING <ls_result> WHERE match IS INITIAL AND packmove IS INITIAL.

      lv_package = lo_folder_logic->path_to_package(
        iv_top  = io_repo->get_package( )
        io_dot  = io_repo->get_dot_abapgit( )
        iv_path = <ls_result>-path
        iv_create_if_not_exists = abap_false ).

      ls_tadir = Lcl_abapgit_factory=>get_tadir( )->read_single(
        iv_object   = <ls_result>-obj_type
        iv_obj_name = <ls_result>-obj_name ).

      IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> lv_package.
* overwriting object from different package than expected
        CLEAR ls_overwrite.
        CONCATENATE <ls_result>-lstate <ls_result>-rstate INTO ls_overwrite-state RESPECTING BLANKS.
        REPLACE ALL OCCURRENCES OF ` ` IN ls_overwrite-state WITH '_'.
        ls_overwrite-obj_type = <ls_result>-obj_type.
        ls_overwrite-obj_name = <ls_result>-obj_name.
        ls_overwrite-devclass = ls_tadir-devclass.
        ls_overwrite-action   = Lif_abapgit_objects=>c_deserialize_action-overwrite.
        ls_overwrite-icon     = icon_change.
        ls_overwrite-text     = 'Overwrite local object'.
        INSERT ls_overwrite INTO TABLE lt_overwrite_unique.
      ENDIF.

    ENDLOOP.

    rt_overwrite = lt_overwrite_unique.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_CHECK implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_factory===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_factory===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_gui_jumper.

    IF gi_gui_jumper IS INITIAL.
      CREATE OBJECT gi_gui_jumper TYPE Lcl_abapgit_gui_jumper.
    ENDIF.

    ri_gui_jumper = gi_gui_jumper.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_injector==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_injector==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_INJECTOR implementation.
*"* method's implementations
*include methods.
  METHOD set_gui_jumper.

    Lcl_abapgit_objects_factory=>gi_gui_jumper = ii_gui_jumper.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_INJECTOR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_super=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_super=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECTS_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.
  METHOD corr_insert.

    DATA: lv_object       TYPE trobj_name,
          lv_object_class TYPE tadir-object.

    IF ig_object_class IS NOT INITIAL.
      lv_object_class = ig_object_class.
      IF ig_object_class = 'DICT'.
        CONCATENATE ms_item-obj_type ms_item-obj_name INTO lv_object.
      ELSE.
        lv_object = ms_item-obj_name.
      ENDIF.
    ELSE.
      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.
    ENDIF.

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = lv_object_class
      iv_obj_name = lv_object
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.
  METHOD delete_ddic.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_objtype TYPE rsedd0-ddobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = iv_objtype.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
            no_ask_delete_append = iv_no_ask_delete_append
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
      CATCH cx_sy_dyn_call_param_not_found.
        TRY.
            " try to force deletion for APPENDs
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = lv_objname
                objtype              = lv_objtype
                aie_force_deletion   = iv_no_ask_delete_append
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6.
          CATCH cx_sy_dyn_call_param_not_found.
            " no_ask_delete_append and aie_force_deletion not available in lower releases
            CALL FUNCTION 'RS_DD_DELETE_OBJ'
              EXPORTING
                no_ask               = iv_no_ask
                objname              = lv_objname
                objtype              = lv_objtype
              EXCEPTIONS
                not_executed         = 1
                object_not_found     = 2
                object_not_specified = 3
                permission_failure   = 4
                dialog_needed        = 5
                OTHERS               = 6.
        ENDTRY.
    ENDTRY.

    IF sy-subrc = 5.
      Lcx_abapgit_exception=>raise( |Object { ms_item-obj_type } { ms_item-obj_name
                                    } has dependencies and must be deleted manually| ).
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.
  METHOD delete_longtexts.

    Lcl_abapgit_factory=>get_longtexts( )->delete(
      iv_longtext_id = iv_longtext_id
      iv_object_name = ms_item-obj_name ).

  ENDMETHOD.
  METHOD deserialize_longtexts.

    Lcl_abapgit_factory=>get_longtexts( )->deserialize(
      ii_xml           = ii_xml
      iv_longtext_name = iv_longtext_name
      iv_object_name   = ms_item-obj_name
      iv_longtext_id   = iv_longtext_id
      iv_main_language = mv_language ).

  ENDMETHOD.
  METHOD exists_a_lock_entry_for.

    DATA: lt_lock_entries TYPE STANDARD TABLE OF seqg3.
    DATA: lv_argument TYPE seqg3-garg.

    IF iv_prefix IS INITIAL.
      lv_argument = iv_argument.
    ELSE.
      lv_argument = |{ iv_prefix  }{ iv_argument }|.
      OVERLAY lv_argument WITH '                                          '.
      lv_argument = lv_argument && '*'.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname                = '*'
        garg                  = lv_argument
      TABLES
        enq                   = lt_lock_entries
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_lock_entries TRANSPORTING NO FIELDS
                               WITH KEY gobj = iv_lock_object.
    IF sy-subrc = 0.
      rv_exists_a_lock_entry = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD get_metadata.

    DATA: lv_class TYPE string.

    lv_class = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).

    REPLACE FIRST OCCURRENCE OF 'LCL_ABAPGIT' IN lv_class WITH 'LCL'.

    rs_metadata-class = lv_class.
    rs_metadata-version = 'v1.0.0'.

  ENDMETHOD.
  METHOD is_active.

    rv_active = Lcl_abapgit_objects_activation=>is_active( ms_item ).

  ENDMETHOD.
  METHOD serialize_longtexts.

    Lcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_object_name   = ms_item-obj_name
      iv_longtext_name = iv_longtext_name
      iv_longtext_id   = iv_longtext_id
      it_dokil         = it_dokil
      io_i18n_params   = mo_i18n_params
      ii_xml           = ii_xml ).

  ENDMETHOD.
  METHOD set_default_package.

    " In certain cases we need to set the package via ABAP memory
    " because we can't supply it via the APIs.
    "
    " Set default package, see function module RS_CORR_INSERT FORM get_current_devclass.
    "
    " We use ABAP memory instead the SET parameter because it is
    " more reliable. SET parameter doesn't work when multiple objects
    " are deserialized which uses the ABAP memory mechanism.
    " We don't need to reset the memory as it is done in above mentioned form routine.

    EXPORT current_devclass FROM iv_package TO MEMORY ID 'EUK'.

  ENDMETHOD.
  METHOD set_default_transport.

    " In certain cases we need to set the transport via ABAP memory
    " because we can't supply it via the APIs.
    "
    " See function module RS_CORR_INSERT

    EXPORT tasknr FROM iv_transport TO MEMORY ID 'EUT'.

  ENDMETHOD.
  METHOD tadir_delete.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_delete_tadir_entry          = abap_true
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = ms_item-obj_type
        wi_tadir_obj_name              = ms_item-obj_name
        wi_test_modus                  = abap_false
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD tadir_insert.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus                  = abap_false
        wi_tadir_pgmid                 = 'R3TR'
        wi_tadir_object                = ms_item-obj_type
        wi_tadir_obj_name              = ms_item-obj_name
        wi_tadir_author                = sy-uname
        wi_tadir_devclass              = iv_package
        wi_tadir_masterlang            = mv_language
        iv_delflag                     = abap_false
      EXCEPTIONS
        tadir_entry_not_existing       = 1
        tadir_entry_ill_type           = 2
        no_systemname                  = 3
        no_systemtype                  = 4
        original_system_conflict       = 5
        object_reserved_for_devclass   = 6
        object_exists_global           = 7
        object_exists_local            = 8
        object_is_distributed          = 9
        obj_specification_not_unique   = 10
        no_authorization_to_delete     = 11
        devclass_not_existing          = 12
        simultanious_set_remove_repair = 13
        order_missing                  = 14
        no_modification_of_head_syst   = 15
        pgmid_object_not_allowed       = 16
        masterlanguage_not_specified   = 17
        devclass_not_specified         = 18
        specify_owner_unique           = 19
        loc_priv_objs_no_repair        = 20
        gtadir_not_reached             = 21
        object_locked_for_order        = 22
        change_of_class_not_allowed    = 23
        no_change_from_sap_to_tmp      = 24
        OTHERS                         = 25.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD clear_abap_language_version.

    " Used during serializing of objects
    IF ms_item-abap_language_version <> Lcl_abapgit_abap_language_vers=>c_any_abap_language_version.
      " ABAP language is defined in repo setting so there's no need to serialize it
      CLEAR cv_abap_language_version.
    ENDIF.

  ENDMETHOD.
  METHOD set_abap_language_version.

    " Used during deserializing of objects
    IF ms_item-abap_language_version <> Lcl_abapgit_abap_language_vers=>c_any_abap_language_version.
      " ABAP language is defined in repo setting so set it accordingly
      cv_abap_language_version = ms_item-abap_language_version.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECTS_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AIFC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_aifc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_aifc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AIFC implementation.
*"* method's implementations
*include methods.
  METHOD authorization_check.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    rv_success = abap_false.
    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~AUTHORIZATION_CHECK')
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD clear_client.
    DATA:
      BEGIN OF ls_data_to_clear,
        mandt  TYPE sy-mandt,
        client TYPE sy-mandt,
      END OF ls_data_to_clear.

    FIELD-SYMBOLS:
      <ls_data> TYPE any.

    LOOP AT ct_data ASSIGNING <ls_data>.
      MOVE-CORRESPONDING ls_data_to_clear TO <ls_data>.
    ENDLOOP.
  ENDMETHOD.
  METHOD compress_interface.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~COMPRESS_INTERFACE')
          EXPORTING
            is_ifkeys  = is_ifkeys
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD constructor.
    DATA: lx_exc_ref TYPE REF TO cx_sy_dyn_call_error.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    ms_icd_data_key = is_item-obj_name.

    TRY.
        CALL METHOD ('/AIF/CL_ABAPGIT_AIFC_UTIL')=>('GET_INSTANCE')
          RECEIVING
            rr_abapgit_aifc_util = mo_abapgit_util.

      CATCH cx_sy_dyn_call_error INTO lx_exc_ref.
        Lcx_abapgit_exception=>raise( 'AIFC not supported' ).
    ENDTRY.
  ENDMETHOD.
  METHOD execute_checks.
    DATA ls_ifkeys TYPE ty_aif_key_s.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = '/AIF/T_FINF' ).
    lr_tabledescr = cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <lt_table>.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Fieldsymbol not assigned' ).
    ENDIF.

    TRY.
        io_xml->read( EXPORTING
                    iv_name = '/AIF/T_FINF'
                  CHANGING
                    cg_data = <lt_table> ).

        READ TABLE <lt_table> ASSIGNING <ls_table> INDEX 1.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'NS' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ns = <lv_value>.
          ENDIF.

          ASSIGN COMPONENT 'IFNAME' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ifname = <lv_value>.
          ENDIF.

          ASSIGN COMPONENT 'IFVERSION' OF STRUCTURE <ls_table> TO <lv_value>.
          IF sy-subrc = 0.
            ls_ifkeys-ifver = <lv_value>.
          ENDIF.

          CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~EXECUTE_CHECKS')
            EXPORTING
              is_ifkeys  = ls_ifkeys
              is_finf    = <ls_table>
            RECEIVING
              rv_success = rv_success.
        ENDIF.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD get_content_compress.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lo_log TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_log TYPE ('/AIF/CL_ABAPGIT_BAL_LOG')
          EXPORTING ir_git_log = io_log
                    is_item = ms_item.

        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~INITIALIZE_CONTENT_COMPRESS')
          EXPORTING
            ir_bal     = lo_log
            is_ifkey   = is_ifkeys
            iv_package = iv_package
            iv_depl_id = ms_icd_data_key-depl_scenario.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD handle_table_data.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~HANDLE_TABLE_DATA')
          EXPORTING
            iv_tabname = iv_tabname
            it_data    = it_data.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD validate_interface.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.
    DATA: lx_root TYPE REF TO cx_root.

    rv_success = abap_false.
    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~VALIDATE_INTERFACE')
          EXPORTING
            is_ifkeys  = is_ifkeys
          RECEIVING
            rv_success = rv_success.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~CHANGED_BY')
          EXPORTING
            is_key  = ls_icd_data_key
          RECEIVING
            rv_user = rv_user.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    Lcx_abapgit_exception=>raise( 'Delete not supported.' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lt_content TYPE ty_content_t.

    DATA lr_tabledescr TYPE REF TO cl_abap_tabledescr.
    DATA lr_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lr_table TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <ls_table> TYPE any.

    DATA ls_ifkey TYPE ty_aif_key_s.
    DATA lr_content TYPE REF TO ty_content_s.

    DATA lx_abap_not_a_table TYPE REF TO cx_abap_not_a_table.

    DATA lv_tablename TYPE string.
    FIELD-SYMBOLS: <lv_value> TYPE any.

    IF iv_step <> Lif_abapgit_object=>gc_step_id-abap.
      RETURN.
    ENDIF.

    TRY.
        IF execute_checks( io_xml ) = abap_false.
          Lcx_abapgit_exception=>raise( 'AIF interface checks failed' ).
        ENDIF.

        io_xml->read( EXPORTING
                        iv_name = `Content_table`
                      CHANGING
                        cg_data = lt_content ).


        LOOP AT lt_content REFERENCE INTO lr_content.
          TRY.
              lv_tablename = cl_abap_dyn_prg=>check_table_name_str( val = lr_content->tabname
                                                                    packages = '' ).
            CATCH cx_abap_not_a_table INTO lx_abap_not_a_table.
              Lcx_abapgit_exception=>raise_with_text( lx_abap_not_a_table ).
            CATCH cx_abap_not_in_package.
              "thats fine
          ENDTRY.

          CLEAR lr_tabledescr.
          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( p_name = lr_content->tabname ).
          lr_tabledescr = cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).

          CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
          ASSIGN lr_table->* TO <lt_table>.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise( 'Fieldsymbol not assigned' ).
          ENDIF.

          io_xml->read( EXPORTING
                          iv_name = lr_content->tabname
                        CHANGING
                          cg_data = <lt_table> ).

          handle_table_data( iv_tabname = lr_content->tabname
                             it_data = <lt_table> ).

          IF lr_content->tabname = '/AIF/T_FINF'.
            READ TABLE <lt_table> ASSIGNING <ls_table> INDEX 1.

            ASSIGN COMPONENT 'NS' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ns = <lv_value>.
              UNASSIGN <lv_value>.
            ENDIF.

            ASSIGN COMPONENT 'IFNAME' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ifname = <lv_value>.
              UNASSIGN <lv_value>.
            ENDIF.

            ASSIGN COMPONENT 'IFVERSION' OF STRUCTURE <ls_table> TO <lv_value>.
            IF <lv_value> IS ASSIGNED.
              ls_ifkey-ifver = <lv_value>.
              UNASSIGN <lv_value>.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF ls_ifkey IS INITIAL.
          RETURN.
        ENDIF.

        get_content_compress( io_log = ii_log
                              is_ifkeys = ls_ifkey
                              iv_package = iv_package ).


        IF authorization_check( ) = abap_false.
          RETURN.
        ENDIF.

        IF validate_interface( ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

        IF compress_interface( ls_ifkey ) = abap_false.
          RETURN.
        ENDIF.

      CATCH cx_root INTO lx_root.
        ii_log->add_exception( ix_exc = lx_root
                               is_item = ms_item ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.

    ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
    ls_icd_data_key-ns = ms_icd_data_key-ns.
    ls_icd_data_key-ifname = ms_icd_data_key-ifname.
    ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

    rv_bool = abap_false.

    TRY.
        CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~EXISTS')
          EXPORTING
            is_key  = ls_icd_data_key
          RECEIVING
            rv_bool = rv_bool.

      CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
        Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_false.
    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    TYPES: ty_rsparamsl_255_t TYPE STANDARD TABLE OF rsparamsl_255 WITH NON-UNIQUE DEFAULT KEY.

    DATA lv_report TYPE progname VALUE '/AIF/CONTENT_DISPLAY'.
    DATA lt_params TYPE ty_rsparamsl_255_t.
    DATA ls_param LIKE LINE OF lt_params.

    ls_param-selname = 'P_DEPL'.
    ls_param-kind = 'P'.
    ls_param-sign = 'I'.
    ls_param-option = 'EQ'.
    ls_param-low = ms_icd_data_key-depl_scenario.
    APPEND ls_param TO lt_params.

    SUBMIT (lv_report) WITH SELECTION-TABLE lt_params AND RETURN.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    DATA: lx_root TYPE REF TO cx_root.
    DATA: lx_dyn_call_error TYPE REF TO cx_sy_dyn_call_error.

    DATA ls_icd_data_key TYPE ty_icd_data_key.
    DATA lt_ifdata TYPE ty_table_data_t.

    DATA lr_data TYPE REF TO data.
    FIELD-SYMBOLS <ls_data> TYPE any.

    DATA lt_content TYPE ty_content_t.
    DATA ls_content TYPE ty_content_s.
    DATA lr_ifdata TYPE REF TO ty_table_data_s.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.

    TRY.

        ASSIGN lr_data TO <ls_data>.
        IF NOT <ls_data> IS ASSIGNED.
          RETURN.
        ENDIF.

        ls_icd_data_key-depl_scenario = ms_icd_data_key-depl_scenario.
        ls_icd_data_key-ns = ms_icd_data_key-ns.
        ls_icd_data_key-ifname = ms_icd_data_key-ifname.
        ls_icd_data_key-ifver2 = ms_icd_data_key-ifver2.

        TRY.
            CALL METHOD mo_abapgit_util->('/AIF/IF_ABAPGIT_AIFC_UTIL~GET_IF_DATA')
              EXPORTING
                is_key    = ls_icd_data_key
              RECEIVING
                rt_ifdata = lt_ifdata.

          CATCH cx_sy_dyn_call_error INTO lx_dyn_call_error.
            Lcx_abapgit_exception=>raise( iv_text = 'AIFC not supported'
                                          ix_previous = lx_dyn_call_error ).
        ENDTRY.

        LOOP AT lt_ifdata REFERENCE INTO lr_ifdata.

          UNASSIGN <lt_table>.
          ASSIGN lr_ifdata->table_data->* TO <lt_table>.
          IF <lt_table> IS NOT ASSIGNED.
            CONTINUE.
          ENDIF.

          clear_client( CHANGING ct_data = <lt_table> ).

          io_xml->add( iv_name = lr_ifdata->tabname
                       ig_data = <lt_table> ).

          ls_content-tabname = lr_ifdata->tabname.
          APPEND ls_content TO lt_content.

        ENDLOOP.

        io_xml->add( iv_name = `Content_table`
                     ig_data = lt_content ).

      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise( iv_text = 'Serialize not possible'
                                      ix_previous = lx_dyn_call_error ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AIFC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AREA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_area=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_area=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AREA implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_user TYPE string.

    SELECT SINGLE tstpnm FROM ('RSDAREA') INTO lv_user.

    rv_user = lv_user.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lr_area         TYPE REF TO object.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~DELETE_NODE')
      EXPORTING
        i_nodename    = ms_item-obj_name
        i_with_dialog = ''.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while deleting AREA: { ms_item-obj_name }| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lv_nodename   TYPE c LENGTH 40,
      lv_parentname TYPE c LENGTH 40,
      lv_txtsh      TYPE c LENGTH 20,
      lv_txtlg      TYPE c LENGTH 60,
      lr_area       TYPE REF TO object.

    io_xml->read( EXPORTING iv_name = 'NODENAME'
                  CHANGING cg_data = lv_nodename ).

    io_xml->read( EXPORTING iv_name = 'PARENTNAME'
                  CHANGING  cg_data = lv_parentname ).

    io_xml->read( EXPORTING iv_name = 'TXTSH'
                  CHANGING  cg_data = lv_txtsh ).

    io_xml->read( EXPORTING iv_name = 'TXTLG'
                  CHANGING  cg_data = lv_txtlg ).

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~CREATE_NODE')
      EXPORTING
        i_parentname = lv_parentname
        i_nodename   = lv_nodename
        i_txtsh      = lv_txtsh
        i_txtlg      = lv_txtlg.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while creating AREA: { ms_item-obj_name }| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_tree> TYPE STANDARD TABLE,
      <ls_tree> TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = ''
        i_langu   = ''
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    IF sy-subrc = 0.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_tree> TYPE STANDARD TABLE,
      <ls_tree> TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = 'A'
        i_langu   = mv_language
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    IF sy-subrc = 0.
      rv_active = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( 'ERSDAREA' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data,
      lr_rsdareat TYPE REF TO data,
      lv_select   TYPE string.

    FIELD-SYMBOLS:
      <lt_tree>       TYPE STANDARD TABLE,
      <ls_tree>       TYPE any,
      <lv_parentname> TYPE any,
      <ls_rsdareat>   TYPE any,
      <lv_txtlg>      TYPE any,
      <lv_txtsh>      TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CREATE DATA lr_rsdareat TYPE ('RSDAREAT').
    ASSIGN lr_rsdareat->* TO <ls_rsdareat>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = 'A'
        i_langu   = mv_language
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    lv_select = |INFOAREA = '{ ms_item-obj_name }'|.

    SELECT SINGLE * FROM ('RSDAREAT')
    INTO <ls_rsdareat>
    WHERE infoarea = ms_item-obj_name.

    ASSIGN COMPONENT 'TXTSH' OF STRUCTURE <ls_rsdareat> TO <lv_txtsh>.
    ASSIGN COMPONENT 'TXTLG' OF STRUCTURE <ls_rsdareat> TO <lv_txtlg>.


    ASSIGN COMPONENT 'PARENTNAME' OF STRUCTURE <ls_tree> TO <lv_parentname>.

    io_xml->add( iv_name = 'NODENAME'
                 ig_data = ms_item-obj_name ).

    io_xml->add( iv_name = 'PARENTNAME'
                 ig_data = <lv_parentname> ).

    io_xml->add( iv_name = 'TXTSH'
                 ig_data = <lv_txtsh> ).

    io_xml->add( iv_name = 'TXTLG'
                 ig_data = <lv_txtlg> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AREA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ASFC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_asfc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_asfc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ASFC implementation.
*"* method's implementations
*include methods.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        is_item     = ms_item
        iv_language = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    set_default_transport( iv_transport ).

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    set_default_transport( iv_transport ).

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ASFC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_AUTH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_auth=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_auth=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_AUTH implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_fieldname = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " there is a bug in SAP standard, the TADIR entries are not deleted
    " when the AUTH object is deleted in transaction SU20

    " FM SUSR_AUTF_DELETE_FIELD calls the UI, therefore we reimplement its logic

    DATA:
      lt_objlst TYPE susr_t_xuobject,
      lo_auth   TYPE REF TO cl_auth_tools.

    " authority check
    CREATE OBJECT lo_auth.
    IF lo_auth->authority_check_suso( actvt     = '06'
                                      fieldname = mv_fieldname ) <> 0.
      MESSAGE e463(01) WITH mv_fieldname INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " if field is used check
    lt_objlst = lo_auth->suso_where_used_afield( mv_fieldname ).
    IF lt_objlst IS NOT INITIAL.
      MESSAGE i453(01) WITH mv_fieldname INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " collect fieldname into a transport task
    IF lo_auth->add_afield_to_trkorr( mv_fieldname ) <> 0.
      "no transport -> no deletion
      MESSAGE e507(0m) INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE FROM authx WHERE fieldname = mv_fieldname.
    IF sy-subrc <> 0.
      MESSAGE e507(0m) INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see include LSAUT_FIELDF02

    DATA: ls_authx TYPE authx,
          lo_auth  TYPE REF TO cl_auth_tools.


    io_xml->read( EXPORTING iv_name = 'AUTHX'
                  CHANGING cg_data = ls_authx ).

    tadir_insert( iv_package ).

    CREATE OBJECT lo_auth.

    IF lo_auth->add_afield_to_trkorr( ls_authx-fieldname ) <> 0.
      Lcx_abapgit_exception=>raise( 'Error deserializing AUTH' ).
    ENDIF.

    MODIFY authx FROM ls_authx.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error deserializing AUTH' ).
    ENDIF.

    CALL FUNCTION 'DB_COMMIT'.
    lo_auth->set_authfld_info_from_db( ls_authx-fieldname ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    SELECT SINGLE fieldname FROM authx
      INTO mv_fieldname
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    IF Lcl_abapgit_factory=>get_function_module( )->function_exists( 'SU20_MAINTAIN_SNGL' ) = abap_true.
      " this function module does not exist in 740
      CALL FUNCTION 'SU20_MAINTAIN_SNGL'
        EXPORTING
          id_field    = mv_fieldname
          id_wbo_mode = abap_false.
      rv_exit = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_authx TYPE authx.


    SELECT SINGLE * FROM authx INTO ls_authx
      WHERE fieldname = ms_item-obj_name.               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'AUTHX'
                 ig_data = ls_authx ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_AUTH implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_BDEF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_bdef=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_bdef=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_BDEF implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_metadata
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    FIELD-SYMBOLS: <lv_links> TYPE ANY TABLE.
    FIELD-SYMBOLS: <lv_value> TYPE data.
    FIELD-SYMBOLS <ls_item> TYPE any.

    clear_field(
      EXPORTING
        iv_fieldname          = 'VERSION'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CREATED_AT'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CREATED_BY'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CHANGED_AT'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'CHANGED_BY'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'RESPONSIBLE'
      CHANGING
        cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'PACKAGE_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'CONTAINER_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MASTER_SYSTEM'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CHANGED_AT'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CHANGED_BY'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CREATED_AT'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CREATED_BY'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-RESPONSIBLE'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-PACKAGE_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-CONTAINER_REF'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'MAIN_OBJECT-MASTER_SYSTEM'
      CHANGING
      cs_metadata = cs_metadata ).

    clear_field(
      EXPORTING
      iv_fieldname          = 'SYNTAX_CONFIGURATION'
      CHANGING
      cs_metadata = cs_metadata ).

    ASSIGN COMPONENT 'LINKS' OF STRUCTURE cs_metadata TO <lv_links>.
    ASSERT sy-subrc = 0.

    LOOP AT <lv_links> ASSIGNING <ls_item>.
      ASSIGN COMPONENT 'COMMON_ATTRIBUTES' OF STRUCTURE <ls_item> TO <lv_value>.
      ASSERT sy-subrc = 0.
      CLEAR: <lv_value>.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_behaviour_definition_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_behaviour_definition TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_BDEF_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |BDEF not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_object_data.

    DATA:
      lr_metadata TYPE REF TO data,
      lr_data     TYPE REF TO data.

    FIELD-SYMBOLS:
      <lv_metadata_node> TYPE any,
      <ls_metadata>      TYPE any,
      <lv_source>        TYPE any,
      <lg_data>          TYPE any.

    CREATE DATA lr_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_data->* TO <lg_data>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'METADATA' OF STRUCTURE <lg_data> TO <lv_metadata_node>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_metadata  TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA-METADATA').
    ASSIGN lr_metadata->* TO <ls_metadata>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'BDEF'
      CHANGING
        cg_data = <ls_metadata> ).

    <lv_metadata_node> = <ls_metadata>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <lg_data> TO <lv_source>.
    ASSERT sy-subrc = 0.

    <lv_source> = Lif_abapgit_object~mo_files->read_string( 'asbdef' ).

    CREATE OBJECT ro_object_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').

    ro_object_data->set_data( p_data = <lg_data> ).

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'BDEF'.
    ls_object_type-subtype_wb = 'BDO'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_behaviour_definition_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

  ENDMETHOD.
  METHOD merge_object_data.

    DATA:
      lo_object_data        TYPE REF TO object,
      lo_object_data_old    TYPE REF TO if_wb_object_data_model,
      lr_new                TYPE REF TO data,
      lr_old                TYPE REF TO data,
      lo_wb_object_operator TYPE REF TO object.

    FIELD-SYMBOLS:
      <ls_new>       TYPE any,
      <ls_old>       TYPE any,
      <lv_field_old> TYPE any,
      <lv_field_new> TYPE any.

    CREATE OBJECT lo_object_data TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_BLUE_SOURCE_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_old->* TO <ls_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD lo_object_data->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL'
      IMPORTING
        p_data           = <ls_new>.

    lo_wb_object_operator = get_wb_object_operator( ).

    CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      EXPORTING
        data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        eo_object_data = lo_object_data_old.

    CALL METHOD lo_object_data_old->('GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        p_data           = <ls_old>.

    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    CREATE OBJECT ro_object_data_merged TYPE ('CL_BLUE_SOURCE_OBJECT_DATA').

    CALL METHOD ro_object_data_merged->('SET_DATA')
      EXPORTING
        p_data = <ls_old>.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lo_object_data_merged TYPE REF TO if_wb_object_data_model,
      lo_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lr_wbobjtype          TYPE REF TO data,
      lr_category           TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_wbobjtype> TYPE any,
      <lv_category>  TYPE any,
      <lv_field>     TYPE any.

    TRY.

        lo_object_data = get_object_data( io_xml ).

        CREATE DATA lr_wbobjtype TYPE ('WBOBJTYPE').
        ASSIGN lr_wbobjtype->* TO <ls_wbobjtype>.
        ASSIGN COMPONENT 'OBJTYPE_TR' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'BDEF'.
        ASSIGN COMPONENT 'SUBTYPE_WB' OF STRUCTURE <ls_wbobjtype> TO <lv_field>.
        <lv_field> = 'BDO'.

        CREATE DATA lr_category TYPE ('WBADT_RESOURCE_CATEGORY').
        ASSIGN lr_category->* TO <lv_category>.

        CALL METHOD ('CL_BLUE_WB_UTILITY')=>('GET_RESOURCE_CATEGORY')
          EXPORTING
            is_object_type = <ls_wbobjtype>
          RECEIVING
            result         = <lv_category>.

        lo_wb_object_operator = get_wb_object_operator( ).

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_false.
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'AL' " if_wb_object_data_selection_co=>c_all_data
                  version           = 'I'
                  package           = iv_package
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
                  version           = 'I'
                  package           = iv_package
                  transport_request = iv_transport.

              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN OTHERS.
          ENDCASE.
        ELSE.
          lo_object_data_merged = merge_object_data( lo_object_data ).
          CASE <lv_category>.
            WHEN '1'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN '2'. "if_wb_adt_plugin_resource_co=>co_sfs_res_category_compound_s.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'P' "if_wb_object_data_selection_co=>c_properties
                  version           = 'I'
                  transport_request = iv_transport.
              CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
                EXPORTING
                  io_object_data    = lo_object_data_merged
                  data_selection    = 'D' "if_wb_object_data_selection_co=>c_data_content
                  version           = 'I'
                  transport_request = iv_transport.
            WHEN OTHERS.
          ENDCASE.
        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key              = mv_behaviour_definition_key
            p_version                 = 'A'
            p_existence_check_only    = abap_true ).
        rv_bool = abap_true.

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESDIC'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root,
      lv_source             TYPE string.

    FIELD-SYMBOLS:
      <ls_behaviour_definition> TYPE any,
      <lv_metadata>             TYPE any,
      <lv_source>               TYPE string.

    ASSIGN mr_behaviour_definition->* TO <ls_behaviour_definition>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_behaviour_definition>
            eo_object_data = li_object_data_model.

        ASSIGN COMPONENT 'METADATA' OF STRUCTURE <ls_behaviour_definition> TO <lv_metadata>.
        ASSERT sy-subrc = 0.
        clear_fields( CHANGING cs_metadata = <lv_metadata> ).

        ASSIGN COMPONENT 'CONTENT-SOURCE' OF STRUCTURE <ls_behaviour_definition> TO <lv_source>.
        ASSERT sy-subrc = 0.
        lv_source = <lv_source>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
        iv_name = 'BDEF'
        ig_data = <lv_metadata> ).

    Lif_abapgit_object~mo_files->add_string(
        iv_ext    = 'asbdef'
        iv_string = lv_source ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_BDEF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CLAS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_clas=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_clas=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CLAS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    CREATE OBJECT mi_object_oriented_object_fct TYPE Lcl_abapgit_oo_class.

    mv_classpool_name = cl_oo_classname_service=>get_classpool_name( |{ is_item-obj_name }| ).

  ENDMETHOD.
  METHOD deserialize_abap.

    DATA: ls_vseoclass             TYPE vseoclass,
          lt_source                TYPE seop_source_string,
          lt_local_definitions     TYPE seop_source_string,
          lt_local_implementations TYPE seop_source_string,
          lt_local_macros          TYPE seop_source_string,
          lt_test_classes          TYPE seop_source_string,
          lt_descriptions          TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lt_descriptions_sub      TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          ls_class_key             TYPE seoclskey,
          lt_attributes            TYPE Lif_abapgit_definitions=>ty_obj_attribute_tt.


    lt_source = Lif_abapgit_object~mo_files->read_abap( ).

    lt_local_definitions = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_def
      iv_error = abap_false ).

    lt_local_implementations = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_imp
      iv_error = abap_false ).

    lt_local_macros = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-macros
      iv_error = abap_false ).

    lt_test_classes = Lif_abapgit_object~mo_files->read_abap(
      iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-testclasses
      iv_error = abap_false ).

    ls_class_key-clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    set_abap_language_version( CHANGING cv_abap_language_version = ls_vseoclass-unicode ).

    ii_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = lt_attributes ).

    " Remove code for test classes if they have been deleted
    IF ls_vseoclass-with_unit_tests = abap_false.
      CLEAR lt_test_classes.
    ENDIF.

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_true
        iv_package    = iv_package
        it_attributes = lt_attributes
      CHANGING
        cg_properties = ls_vseoclass ).

    mi_object_oriented_object_fct->generate_locals(
      is_key                   = ls_class_key
      iv_package               = iv_package
      iv_version               = ls_vseoclass-unicode
      it_local_definitions     = lt_local_definitions
      it_local_implementations = lt_local_implementations
      it_local_macros          = lt_local_macros
      it_local_test_classes    = lt_test_classes ).

    repo_apack_replacement( CHANGING ct_source = lt_source ).

    mi_object_oriented_object_fct->deserialize_source(
      is_key     = ls_class_key
      iv_package = iv_package
      iv_version = ls_vseoclass-unicode
      it_source  = lt_source ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS'
                  CHANGING cg_data = lt_descriptions ).

    mi_object_oriented_object_fct->update_descriptions(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions ).

    ii_xml->read( EXPORTING iv_name = 'DESCRIPTIONS_SUB'
                  CHANGING cg_data = lt_descriptions_sub ).

    mi_object_oriented_object_fct->update_descriptions_sub(
      is_key          = ls_class_key
      it_descriptions = lt_descriptions_sub ).

    mi_object_oriented_object_fct->add_to_activation_list( ms_item ).

  ENDMETHOD.
  METHOD deserialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lt_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    ii_xml->read( EXPORTING iv_name = 'LINES'
                  CHANGING cg_data = lt_lines ).

    lv_object = ms_item-obj_name.

    IF lines( lt_lines ) = 0.
      mi_object_oriented_object_fct->delete_documentation(
        iv_id          = c_longtext_id-class
        iv_object_name = lv_object
        iv_language    = mv_language ).
      RETURN.
    ENDIF.

    mi_object_oriented_object_fct->create_documentation(
      it_lines       = lt_lines
      iv_id          = c_longtext_id-class
      iv_object_name = lv_object
      iv_language    = mv_language ).

    ii_xml->read( EXPORTING iv_name = 'I18N_LINES'
                  CHANGING cg_data = lt_i18n_lines ).

    LOOP AT lt_i18n_lines INTO ls_i18n_lines.
      mi_object_oriented_object_fct->create_documentation(
        it_lines         = ls_i18n_lines-lines
        iv_id            = c_longtext_id-class
        iv_object_name   = lv_object
        iv_language      = ls_i18n_lines-language
        iv_no_masterlang = abap_true ).
    ENDLOOP.

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

    deserialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-types
      iv_longtext_id   = c_longtext_id-types ).

  ENDMETHOD.
  METHOD deserialize_pre_ddic.

    DATA: ls_vseoclass TYPE vseoclass.

    ii_xml->read( EXPORTING iv_name = 'VSEOCLASS'
                  CHANGING  cg_data = ls_vseoclass ).

    set_abap_language_version( CHANGING cv_abap_language_version = ls_vseoclass-unicode ).

    mi_object_oriented_object_fct->create(
      EXPORTING
        iv_check      = abap_false
        iv_package    = iv_package
      CHANGING
        cg_properties = ls_vseoclass ).

  ENDMETHOD.
  METHOD deserialize_sotr.
    "OTR stands for Online Text Repository
    mi_object_oriented_object_fct->create_sotr(
      iv_object_name = ms_item-obj_name
      iv_package     = iv_package
      ii_xml         = ii_xml ).
  ENDMETHOD.
  METHOD deserialize_tpool.

    DATA: lv_clsname   TYPE seoclsname,
          lt_tpool_ext TYPE Lif_abapgit_definitions=>ty_tpool_tt,
          lt_tpool     TYPE textpool_table.

    ii_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    IF lines( lt_tpool ) = 0.
      RETURN.
    ENDIF.

    lv_clsname = ms_item-obj_name.

    mi_object_oriented_object_fct->insert_text_pool(
      iv_class_name = lv_clsname
      it_text_pool  = lt_tpool
      iv_language   = mv_language ).

  ENDMETHOD.
  METHOD deserialize_tpool_i18n.

    DATA: lv_clsname    TYPE seoclsname,
          lt_tpool      TYPE textpool_table,
          lt_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpool.

    lv_clsname = ms_item-obj_name.

    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_i18n_tpool ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'LANGUAGE'
      CHANGING
        ct_tab = lt_i18n_tpool ).

    LOOP AT lt_i18n_tpool INTO ls_i18n_tpool.
      lt_tpool = read_tpool( ls_i18n_tpool-textpool ).
      mi_object_oriented_object_fct->insert_text_pool(
        iv_class_name = lv_clsname
        it_text_pool  = lt_tpool
        iv_language   = ls_i18n_tpool-language
        iv_state      = 'A' ).
    ENDLOOP.

  ENDMETHOD.
  METHOD interface_replacement.

    DATA lv_tabix TYPE sy-tabix.

    FIELD-SYMBOLS <lv_source> LIKE LINE OF ct_source.

    FIND REGEX '^\s*INTERFACES(:| )\s*' && iv_from_interface && '\s*.' IN TABLE ct_source MATCH LINE lv_tabix.
    IF sy-subrc = 0.
      READ TABLE ct_source ASSIGNING <lv_source> INDEX lv_tabix.
      ASSERT sy-subrc = 0.

      REPLACE FIRST OCCURRENCE OF iv_from_interface IN <lv_source>
                             WITH iv_to_interface IGNORING CASE.

      REPLACE ALL OCCURRENCES OF iv_from_interface && '~descriptor' IN TABLE ct_source
                            WITH iv_to_interface && '~descriptor' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF iv_from_interface && '=>' IN TABLE ct_source
                            WITH iv_to_interface && '=>' IGNORING CASE.
      REPLACE ALL OCCURRENCES OF iv_from_interface && '->' IN TABLE ct_source
                            WITH iv_to_interface && '->' IGNORING CASE.
    ENDIF.

  ENDMETHOD.
  METHOD is_class_locked.

    DATA: lv_argument TYPE seqg3-garg.

    lv_argument = ms_item-obj_name.
    OVERLAY lv_argument WITH '=============================='.
    lv_argument = lv_argument && '*'.

    rv_is_class_locked = exists_a_lock_entry_for( iv_lock_object = 'ESEOCLASS'
                                                  iv_argument    = lv_argument ).

  ENDMETHOD.
  METHOD repo_apack_replacement.

    DATA lv_apack TYPE seoclsname.

    " Check if SAP-version of APACK manifest exists
    SELECT SINGLE clsname INTO lv_apack
      FROM seoclass
      WHERE clsname = Lif_abapgit_apack_definitions=>c_apack_interface_sap.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    " If not, replace with abapGit version
    interface_replacement(
      EXPORTING
        iv_from_interface = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_sap )
        iv_to_interface   = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_cust )
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.
  METHOD serialize_attr.

    DATA: lt_attributes TYPE Lif_abapgit_definitions=>ty_obj_attribute_tt.

    lt_attributes = mi_object_oriented_object_fct->read_attributes( iv_clsname ).
    IF lines( lt_attributes ) = 0.
      RETURN.
    ENDIF.

    ii_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = lt_attributes ).

  ENDMETHOD.
  METHOD serialize_descr.

    DATA: lt_descriptions    TYPE Lif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.
    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    ii_xml->add( iv_name = 'DESCRIPTIONS'
                 ig_data = lt_descriptions ).

  ENDMETHOD.
  METHOD serialize_descr_sub.

    DATA: lt_descriptions    TYPE Lif_abapgit_oo_object_fnc=>ty_seosubcotx_tt,
          lv_language        TYPE spras,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      lv_language = mv_language.
    ENDIF.

    lt_descriptions = mi_object_oriented_object_fct->read_descriptions_sub(
      iv_object_name = iv_clsname
      iv_language    = lv_language ).

    IF lines( lt_descriptions ) = 0.
      RETURN.
    ENDIF.
    " Remove technical languages
    lt_language_filter = mo_i18n_params->build_language_filter( ).
    DELETE lt_descriptions WHERE NOT langu IN lt_language_filter AND langu <> mv_language.

    ii_xml->add( iv_name = 'DESCRIPTIONS_SUB'
                 ig_data = lt_descriptions ).

  ENDMETHOD.
  METHOD serialize_docu.

    DATA: lt_lines      TYPE tlinetab,
          lv_object     TYPE dokhl-object,
          lv_langu      TYPE sy-langu,
          lt_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_lines,
          ls_i18n_lines TYPE Lif_abapgit_lang_definitions=>ty_i18n_line.

    lv_object = iv_clsname.

    lt_lines = mi_object_oriented_object_fct->read_documentation(
      iv_id          = c_longtext_id-class
      iv_object_name = lv_object
      iv_language    = mv_language ).
    IF lines( lt_lines ) > 0.
      ii_xml->add( iv_name = 'LINES'
                   ig_data = lt_lines ).
    ENDIF.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_lines = mi_object_oriented_object_fct->read_documentation(
        iv_id          = c_longtext_id-class
        iv_object_name = lv_object
        iv_language    = lv_langu ).

      IF lines( lt_lines ) > 0.
        CLEAR ls_i18n_lines.
        ls_i18n_lines-language = lv_langu.
        ls_i18n_lines-lines    = lt_lines.
        INSERT ls_i18n_lines INTO TABLE lt_i18n_lines.
      ENDIF.

    ENDLOOP.

    IF lines( lt_i18n_lines ) > 0.
      ii_xml->add( iv_name = 'I18N_LINES'
                   ig_data = lt_i18n_lines ).
    ENDIF.

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-attributes
      iv_longtext_id   = c_longtext_id-attributes ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-methods
      iv_longtext_id   = c_longtext_id-methods ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-events
      iv_longtext_id   = c_longtext_id-events ).

    serialize_longtexts(
      ii_xml           = ii_xml
      iv_longtext_name = c_longtext_name-types
      iv_longtext_id   = c_longtext_id-types ).

  ENDMETHOD.
  METHOD serialize_sotr.
    mi_object_oriented_object_fct->read_sotr(
      iv_object_name = ms_item-obj_name
      io_i18n_params = mo_i18n_params
      ii_xml         = ii_xml ).
  ENDMETHOD.
  METHOD serialize_tpool.

    DATA lt_tpool TYPE textpool_table.

    lt_tpool = mi_object_oriented_object_fct->read_text_pool(
      iv_class_name = iv_clsname
      iv_language   = mv_language ).
    ii_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    rt_tpool = lt_tpool.

  ENDMETHOD.
  METHOD serialize_tpool_i18n.

    DATA: lt_tpool      TYPE textpool_table,
          lv_index      TYPE i,
          lv_langu      TYPE sy-langu,
          lt_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpools,
          ls_i18n_tpool TYPE Lif_abapgit_lang_definitions=>ty_i18n_tpool.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF it_tpool_main.

    DATA lt_tpool_main LIKE SORTED TABLE OF <ls_tpool> WITH UNIQUE KEY id key.

    IF mo_i18n_params->ms_params-main_language_only = abap_true OR lines( it_tpool_main ) = 0.
      RETURN.
    ENDIF.

    " Copy single records to be able to catch duplicate key error
    LOOP AT it_tpool_main ASSIGNING <ls_tpool>.
      INSERT <ls_tpool> INTO TABLE lt_tpool_main.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Inconsistent textpool in { ms_item-obj_type } { ms_item-obj_name }| ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_langu_additional INTO lv_langu.

      lt_tpool = mi_object_oriented_object_fct->read_text_pool(
        iv_class_name = iv_clsname
        iv_language   = lv_langu ).

      LOOP AT lt_tpool ASSIGNING <ls_tpool>.
        lv_index = sy-tabix.
        READ TABLE lt_tpool_main WITH KEY id = <ls_tpool>-id key = <ls_tpool>-key
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_tpool INDEX lv_index.
        ENDIF.
      ENDLOOP.

      IF lines( lt_tpool ) > 0.
        CLEAR ls_i18n_tpool.
        ls_i18n_tpool-language = lv_langu.
        ls_i18n_tpool-textpool = add_tpool( lt_tpool ).
        INSERT ls_i18n_tpool INTO TABLE lt_i18n_tpool.
      ENDIF.

    ENDLOOP.

    IF lines( lt_i18n_tpool ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_i18n_tpool ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_xml.

    DATA: ls_vseoclass        TYPE vseoclass,
          lt_tpool            TYPE textpool_table,
          ls_clskey           TYPE seoclskey,
          lt_langu_additional TYPE Lif_abapgit_lang_definitions=>ty_langus,
          lt_language_filter  TYPE Lif_abapgit_environment=>ty_system_language_filter.

    ls_clskey-clsname = ms_item-obj_name.

    "If class was deserialized with a previous versions of abapGit and current language was different
    "from main language at this time, this call would return SY-LANGU as main language. To fix
    "these objects, set SY-LANGU to main language temporarily.
    Lcl_abapgit_language=>set_current_language( mv_language ).

    TRY.
        ls_vseoclass = mi_object_oriented_object_fct->get_class_properties( ls_clskey ).

        clear_abap_language_version( CHANGING cv_abap_language_version = ls_vseoclass-unicode ).

      CLEANUP.
        Lcl_abapgit_language=>restore_login_language( ).

    ENDTRY.

    Lcl_abapgit_language=>restore_login_language( ).

    IF mv_skip_testclass = abap_true.
      CLEAR ls_vseoclass-with_unit_tests.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT language
      INTO TABLE lt_langu_additional
      FROM d010tinf
      WHERE r3state  = 'A'
        AND prog     = mv_classpool_name
        AND language IN lt_language_filter
        AND language <> mv_language
      ORDER BY language.

    ii_xml->add( iv_name = 'VSEOCLASS'
                 ig_data = ls_vseoclass ).

    lt_tpool = serialize_tpool(
      ii_xml     = ii_xml
      iv_clsname = ls_clskey-clsname ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_tpool_i18n(
        ii_xml              = ii_xml
        it_langu_additional = lt_langu_additional
        it_tpool_main       = lt_tpool
        iv_clsname          = ls_clskey-clsname ).
    ENDIF.

    IF ls_vseoclass-category = seoc_category_exception.
      serialize_sotr( ii_xml ).
    ENDIF.

    SELECT DISTINCT langu
      INTO TABLE lt_langu_additional
      FROM dokhl
      WHERE id     = 'CL'
        AND object = ls_clskey-clsname
        AND langu IN lt_language_filter
        AND langu <> mv_language
      ORDER BY langu.

    serialize_docu( ii_xml              = ii_xml
                    iv_clsname          = ls_clskey-clsname
                    it_langu_additional = lt_langu_additional ).

    serialize_descr( ii_xml     = ii_xml
                     iv_clsname = ls_clskey-clsname ).

    serialize_descr_sub( ii_xml     = ii_xml
                         iv_clsname = ls_clskey-clsname ).

    serialize_attr( ii_xml     = ii_xml
                    iv_clsname = ls_clskey-clsname ).

  ENDMETHOD.
  METHOD source_apack_replacement.

    DATA lv_clsname TYPE seoclsname.

    " Check if abapGit version of APACK manifest is used
    SELECT SINGLE clsname INTO lv_clsname
      FROM seometarel
      WHERE clsname    = ms_item-obj_name
        AND refclsname = Lif_abapgit_apack_definitions=>c_apack_interface_cust
        AND version    = '1'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " If yes, replace with SAP-version
    interface_replacement(
      EXPORTING
        iv_from_interface = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_cust )
        iv_to_interface   = to_lower( Lif_abapgit_apack_definitions=>c_apack_interface_sap )
      CHANGING
        ct_source         = ct_source ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_reposrc,
             unam  TYPE reposrc-unam,
             udat  TYPE reposrc-udat,
             utime TYPE reposrc-utime,
           END OF ty_reposrc.

    DATA: lt_reposrc  TYPE STANDARD TABLE OF ty_reposrc,
          ls_reposrc  LIKE LINE OF lt_reposrc,
          lv_include  TYPE syrepid,
          lt_includes TYPE STANDARD TABLE OF syrepid.

    CASE iv_extra.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_def.
        lv_include = cl_oo_classname_service=>get_ccdef_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        lv_include = cl_oo_classname_service=>get_ccimp_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-macros.
        lv_include = cl_oo_classname_service=>get_ccmac_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-testclasses.
        lv_include = cl_oo_classname_service=>get_ccau_name( |{ ms_item-obj_name }| ).
        INSERT lv_include INTO TABLE lt_includes.
      WHEN OTHERS.
        lt_includes = mi_object_oriented_object_fct->get_includes( ms_item-obj_name ).
    ENDCASE.

    ASSERT lines( lt_includes ) > 0.

    SELECT unam udat utime FROM reposrc
      INTO TABLE lt_reposrc
      FOR ALL ENTRIES IN lt_includes
      WHERE progname = lt_includes-table_line
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ELSE.
      SORT lt_reposrc BY udat DESCENDING utime DESCENDING.
      READ TABLE lt_reposrc INDEX 1 INTO ls_reposrc.
      ASSERT sy-subrc = 0.
      rv_user = ls_reposrc-unam.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: ls_clskey TYPE seoclskey.
    ls_clskey-clsname = ms_item-obj_name.

    corr_insert( iv_package ).

    mi_object_oriented_object_fct->delete( ls_clskey ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    IF iv_step = Lif_abapgit_object=>gc_step_id-abap.

      deserialize_abap( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_tpool( io_xml ).

      IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
        deserialize_tpool_i18n( io_xml ).
      ENDIF.

      deserialize_sotr( ii_xml     = io_xml
                        iv_package = iv_package ).

      deserialize_docu( io_xml ).

    ELSEIF iv_step = Lif_abapgit_object=>gc_step_id-early.

      " If class does not exist, create it
      " so DDIC that depends on it does not fail activation
      IF Lif_abapgit_object~exists( ) = abap_false.
        deserialize_pre_ddic(
          ii_xml     = io_xml
          iv_package = iv_package ).
      ELSE.
        corr_insert( iv_package ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

    rv_bool = mi_object_oriented_object_fct->exists( ls_class_key ).

    " Skip classes generated by DDLS (SADL)
    IF rv_bool = abap_true AND
      mi_object_oriented_object_fct->read_superclass( ls_class_key-clsname ) = 'CL_SADL_GTK_EXPOSURE_MPC'.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-early TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    IF is_class_locked( ) = abap_true OR is_text_locked( mv_classpool_name ) = abap_true.
      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.

    ls_item-obj_type = 'PROG'.

    CASE iv_extra.
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_def.
        ls_item-obj_name = cl_oo_classname_service=>get_ccdef_name( |{ ms_item-obj_name }| ).
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-locals_imp.
        ls_item-obj_name = cl_oo_classname_service=>get_ccimp_name( |{ ms_item-obj_name }| ).
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-macros.
        ls_item-obj_name = cl_oo_classname_service=>get_ccmac_name( |{ ms_item-obj_name }| ).
      WHEN Lif_abapgit_oo_object_fnc=>c_parts-testclasses.
        ls_item-obj_name = cl_oo_classname_service=>get_ccau_name( |{ ms_item-obj_name }| ).
    ENDCASE.

    IF ls_item-obj_name IS NOT INITIAL.
      rv_exit = Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump( ls_item ).
    ENDIF.

    " Otherwise covered by ZCL_ABAPGIT_OBJECTS=>JUMP

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lt_source    TYPE seop_source_string,
          ls_class_key TYPE seoclskey.

    ls_class_key-clsname = ms_item-obj_name.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = abap_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = abap_true.

    lt_source = mi_object_oriented_object_fct->serialize_abap( ls_class_key ).

    source_apack_replacement( CHANGING ct_source = lt_source ).

    Lif_abapgit_object~mo_files->add_abap( lt_source ).

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_def ).
    IF lines( lt_source ) > 0.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_def
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_locals_imp ).
    IF lines( lt_source ) > 0.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-locals_imp
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key            = ls_class_key
      iv_type                 = seop_ext_class_testclasses ).

    mv_skip_testclass = mi_object_oriented_object_fct->get_skip_test_classes( ).
    IF lines( lt_source ) > 0 AND mv_skip_testclass = abap_false.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-testclasses
        it_abap  = lt_source ).
    ENDIF.

    lt_source = mi_object_oriented_object_fct->serialize_abap(
      is_class_key = ls_class_key
      iv_type      = seop_ext_class_macros ).
    IF lines( lt_source ) > 0.
      Lif_abapgit_object~mo_files->add_abap(
        iv_extra = Lif_abapgit_oo_object_fnc=>c_parts-macros
        it_abap  = lt_source ).
    ENDIF.

    serialize_xml( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CLAS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CUS0 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cus0=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cus0=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CUS0 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_activity = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_header TYPE ty_img_activity-header.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_READ'
      EXPORTING
        img_activity        = mv_img_activity
      IMPORTING
        img_activity_header = ls_header.

    rv_user = ls_header-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_DELETE'
      EXPORTING
        img_activity = mv_img_activity
      IMPORTING
        message      = ls_message.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from delete CUS0 { mv_img_activity } S_CUS_IMG_ACTIVITY_DELETE| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_img_activity TYPE ty_img_activity,
          ls_text         LIKE LINE OF ls_img_activity-texts.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS0'
      CHANGING
        cg_data = ls_img_activity ).

    READ TABLE ls_img_activity-texts INTO ls_text
                                     WITH KEY spras = mv_language.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_SAVE'
      EXPORTING
        img_activity  = ls_img_activity-header-activity
        i_docu        = ls_img_activity-header-docu_id
        i_attributes  = ls_img_activity-header-attributes
        i_activity    = ls_img_activity-header-c_activity
        i_description = ls_text
        i_tcode       = ls_img_activity-header-tcode.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_EXISTS'
      EXPORTING
        img_activity = mv_img_activity
      IMPORTING
        message      = ls_message.

    rv_bool = boolc( ls_message IS INITIAL ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    DATA: lv_img_activity TYPE cus_img_ac.

    lv_img_activity = mv_img_activity.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_MAINTAIN'
      EXPORTING
        i_display    = abap_true
      CHANGING
        img_activity = lv_img_activity.

    rv_exit = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_img_activity TYPE ty_img_activity.

    CALL FUNCTION 'S_CUS_IMG_ACTIVITY_READ'
      EXPORTING
        img_activity        = mv_img_activity
      IMPORTING
        img_activity_header = ls_img_activity-header
      TABLES
        img_activity_texts  = ls_img_activity-texts.

    CLEAR: ls_img_activity-header-fuser,
           ls_img_activity-header-fdate,
           ls_img_activity-header-ftime,
           ls_img_activity-header-luser,
           ls_img_activity-header-ldate,
           ls_img_activity-header-ltime.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_img_activity-texts WHERE spras <> mv_language.
    ENDIF.

    SORT ls_img_activity-texts.

    io_xml->add( iv_name = 'CUS0'
                 ig_data = ls_img_activity ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS0 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CUS1 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cus1=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cus1=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CUS1 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_customizing_activity = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_header TYPE ty_customzing_activity-activity_header.

    CALL FUNCTION 'S_CUS_ACTIVITY_READ'
      EXPORTING
        activity        = mv_customizing_activity
      IMPORTING
        activity_header = ls_header.

    rv_user = ls_header-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_ACTIVITY_DELETE'
      EXPORTING
        activity = mv_customizing_activity
      IMPORTING
        message  = ls_message.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from delete CUS1 { mv_customizing_activity } S_CUS_ACTIVITY_DELETE| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_customzing_activity TYPE ty_customzing_activity,
          ls_message             TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS1'
      CHANGING
        cg_data = ls_customzing_activity ).

    CALL FUNCTION 'S_CUS_ACTIVITY_SAVE'
      EXPORTING
        activity                     = ls_customzing_activity-activity_header-act_id
        activity_type                = ls_customzing_activity-activity_header-act_type
        tcode                        = ls_customzing_activity-activity_header-tcode
        customer_exit                = ls_customzing_activity-activity_customer_exit-exit_name
        customer_exit_enhancement    = ls_customzing_activity-activity_customer_exit-enhancement
        customer_exit_implementation = ls_customzing_activity-activity_customer_exit-impl_name
      IMPORTING
        message                      = ls_message
      TABLES
        activity_title               = ls_customzing_activity-activity_title
        objects                      = ls_customzing_activity-objects
        objects_texts                = ls_customzing_activity-objects_title.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from deserialize CUS1 { mv_customizing_activity } S_CUS_ACTIVITY_SAVE| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'S_CUS_ACTIVITY_EXIST'
      EXPORTING
        activity            = mv_customizing_activity
      EXCEPTIONS
        activity_exists_not = 1
        OTHERS              = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    DATA: lt_bdc_data TYPE STANDARD TABLE OF bdcdata.
    FIELD-SYMBOLS: <ls_bdc_data> TYPE bdcdata.

    APPEND INITIAL LINE TO lt_bdc_data ASSIGNING <ls_bdc_data>.
    <ls_bdc_data>-program = 'SAPLS_CUS_ACTIVITY'.
    <ls_bdc_data>-dynpro = '0200'.
    <ls_bdc_data>-dynbegin = 'X'.

    APPEND INITIAL LINE TO lt_bdc_data ASSIGNING <ls_bdc_data>.
    <ls_bdc_data>-fnam = 'CUS_ACTH-ACT_ID'.
    <ls_bdc_data>-fval = mv_customizing_activity.

    APPEND INITIAL LINE TO lt_bdc_data ASSIGNING <ls_bdc_data>.
    <ls_bdc_data>-fnam = 'BDC_OKCODE'.
    <ls_bdc_data>-fval = '=ACT_DISP'.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'S_CUS_ACTIVITY'
      it_bdcdata = lt_bdc_data ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_customzing_activity TYPE ty_customzing_activity.

    CALL FUNCTION 'S_CUS_ACTIVITY_READ'
      EXPORTING
        activity               = mv_customizing_activity
      IMPORTING
        activity_header        = ls_customzing_activity-activity_header
        activity_customer_exit = ls_customzing_activity-activity_customer_exit
      TABLES
        activity_title         = ls_customzing_activity-activity_title
        objects                = ls_customzing_activity-objects
        objects_title          = ls_customzing_activity-objects_title.

    CLEAR: ls_customzing_activity-activity_header-fdatetime,
           ls_customzing_activity-activity_header-fuser,
           ls_customzing_activity-activity_header-ldatetime,
           ls_customzing_activity-activity_header-luser.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_customzing_activity-activity_title WHERE spras <> mv_language.
    ENDIF.

    SORT ls_customzing_activity-activity_title.
    SORT ls_customzing_activity-objects.
    SORT ls_customzing_activity-objects_title.

    io_xml->add( iv_name = 'CUS1'
                 ig_data = ls_customzing_activity ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS1 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_CUS2 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_cus2=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_cus2=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_CUS2 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item = is_item
                        iv_language = iv_language ).

    mv_img_attribute = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_header TYPE ty_customizing_attribute-header.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_READ'
      EXPORTING
        img_attribute    = mv_img_attribute
      IMPORTING
        attribute_header = ls_header.

    rv_user = ls_header-luser.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_message TYPE hier_mess.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_DELETE'
      EXPORTING
        img_attribute = mv_img_attribute
      IMPORTING
        message       = ls_message.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from delete CUS2 { mv_img_attribute } S_CUS_ATTRIBUTES_DELETE| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_customizing_attribute TYPE ty_customizing_attribute,
          ls_message               TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'CUS2'
      CHANGING
        cg_data = ls_customizing_attribute ).

    CALL FUNCTION 'S_CUS_ATTRIBUTES_SAVE'
      EXPORTING
        img_attribute         = ls_customizing_attribute-header
      IMPORTING
        message               = ls_message
      TABLES
        attributes_title      = ls_customizing_attribute-titles
        attributes_countries  = ls_customizing_attribute-countries
        attributes_components = ls_customizing_attribute-components.

    IF ls_message-msgty <> 'S'.
      Lcx_abapgit_exception=>raise( |error from deserialize CUS2 { mv_img_attribute } S_CUS_ATTRIBUTES_SAVE| ).
    ENDIF.

    corr_insert( iv_package ).

    tadir_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_EXIST'
      EXPORTING
        img_attribute         = mv_img_attribute
      EXCEPTIONS
        attributes_exists_not = 1
        OTHERS                = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_customizing_attribute TYPE ty_customizing_attribute.

    CALL FUNCTION 'S_CUS_ATTRIBUTES_READ'
      EXPORTING
        img_attribute                 = mv_img_attribute
      IMPORTING
        attribute_header              = ls_customizing_attribute-header
      TABLES
        attribute_title               = ls_customizing_attribute-titles
        attribute_countries           = ls_customizing_attribute-countries
        attribute_components          = ls_customizing_attribute-components
        attribute_components_variants = ls_customizing_attribute-components_variants.

    CLEAR: ls_customizing_attribute-header-fdatetime,
           ls_customizing_attribute-header-fuser,
           ls_customizing_attribute-header-ldatetime,
           ls_customizing_attribute-header-luser.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      DELETE ls_customizing_attribute-titles WHERE spras <> mv_language.
    ENDIF.

    io_xml->add( iv_name = 'CUS2'
                 ig_data = ls_customizing_attribute ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_CUS2 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DEVC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_devc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_devc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DEVC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( is_item     = is_item
                        iv_language = iv_language ).
    IF is_item-devclass IS NOT INITIAL.
      mv_local_devclass = is_item-devclass.
    ELSE.
      mv_local_devclass = is_item-obj_name.
    ENDIF.
  ENDMETHOD.
  METHOD get_package.
    IF Lif_abapgit_object~exists( ) = abap_true.
      ri_package = load_package( mv_local_devclass ).
    ENDIF.
  ENDMETHOD.
  METHOD is_empty.

    DATA: lv_object_name TYPE tadir-obj_name,
          lt_subpackages TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    lt_subpackages = Lcl_abapgit_factory=>get_sap_package( iv_package_name )->list_subpackages( ).

    IF lines( lt_subpackages ) > 0.
      rv_is_empty = abap_false.
      RETURN.
    ENDIF.

    " Ignore the SOTR if is linked to the current SAP package (DEVC)
    SELECT SINGLE obj_name
           FROM tadir
           INTO lv_object_name
           WHERE pgmid = 'R3TR'
           AND NOT ( ( object = 'DEVC' OR object = 'SOTR' ) AND obj_name = iv_package_name )
           AND devclass = iv_package_name.
    rv_is_empty = boolc( sy-subrc <> 0 ).

  ENDMETHOD.
  METHOD is_local.

    DATA lv_dlvunit TYPE tdevc-dlvunit.

    SELECT SINGLE dlvunit FROM tdevc INTO lv_dlvunit
        WHERE devclass = iv_package_name AND intsys <> 'SAP'.
    IF sy-subrc = 0 AND lv_dlvunit = 'LOCAL'.
      rv_is_local = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD load_package.

    cl_package_factory=>load_package(
      EXPORTING
        i_package_name             = iv_package_name
        i_force_reload             = abap_true
      IMPORTING
        e_package                  = ri_package
      EXCEPTIONS
        object_not_existing        = 1
        unexpected_error           = 2
        intern_err                 = 3
        no_access                  = 4
        object_locked_and_modified = 5
        OTHERS                     = 6 ).
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD remove_obsolete_tadir.

    DATA:
      lv_pack  TYPE devclass,
      lt_pack  TYPE STANDARD TABLE OF devclass,
      ls_tadir TYPE Lif_abapgit_definitions=>ty_tadir,
      lt_tadir TYPE Lif_abapgit_definitions=>ty_tadir_tt,
      ls_item  TYPE Lif_abapgit_definitions=>ty_item.

    " TADIR entries must remain for transportable packages
    IF is_local( iv_package_name ) = abap_false.
      RETURN.
    ENDIF.

    " Clean-up sub packages first
    SELECT devclass FROM tdevc INTO TABLE lt_pack
      WHERE parentcl = iv_package_name
      ORDER BY PRIMARY KEY.

    LOOP AT lt_pack INTO lv_pack.
      remove_obsolete_tadir( lv_pack ).
    ENDLOOP.

    " Remove TADIR entries for objects that do not exist anymore
    SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE lt_tadir
      WHERE devclass = iv_package_name
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_tadir INTO ls_tadir.
      ls_item-obj_type = ls_tadir-object.
      ls_item-obj_name = ls_tadir-obj_name.

      IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
        CALL FUNCTION 'TR_TADIR_INTERFACE'
          EXPORTING
            wi_delete_tadir_entry = abap_true
            wi_tadir_pgmid        = 'R3TR'
            wi_tadir_object       = ls_tadir-object
            wi_tadir_obj_name     = ls_tadir-obj_name
            wi_test_modus         = abap_false
          EXCEPTIONS
            OTHERS                = 1 ##FM_SUBRC_OK.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD set_lock.

    DATA: lv_changeable TYPE abap_bool.

    ii_package->get_changeable( IMPORTING e_changeable = lv_changeable ).
    IF lv_changeable <> iv_lock.
      TRY.
          CALL METHOD ii_package->('SET_CHANGEABLE')
            EXPORTING
              i_changeable                = iv_lock
              i_suppress_dialog           = abap_true " Parameter missing in 702
            EXCEPTIONS
              object_locked_by_other_user = 1
              permission_failure          = 2
              object_already_changeable   = 3
              object_already_unlocked     = 4
              object_just_created         = 5
              object_deleted              = 6
              object_modified             = 7
              object_not_existing         = 8
              object_invalid              = 9
              unexpected_error            = 10
              OTHERS                      = 11.
        CATCH cx_sy_dyn_call_param_not_found.
          ii_package->set_changeable(
            EXPORTING
              i_changeable                = iv_lock
            EXCEPTIONS
              object_locked_by_other_user = 1
              permission_failure          = 2
              object_already_changeable   = 3
              object_already_unlocked     = 4
              object_just_created         = 5
              object_deleted              = 6
              object_modified             = 7
              object_not_existing         = 8
              object_invalid              = 9
              unexpected_error            = 10
              OTHERS                      = 11 ).
      ENDTRY.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    TRY.
        CALL METHOD ii_package->('SET_PERMISSIONS_CHANGEABLE')
          EXPORTING
            i_changeable                = iv_lock
            i_suppress_dialog           = abap_true " Parameter missing in 702
          EXCEPTIONS
            object_already_changeable   = 1
            object_already_unlocked     = 2
            object_locked_by_other_user = 3
            object_modified             = 4
            object_just_created         = 5
            object_deleted              = 6
            permission_failure          = 7
            object_invalid              = 8
            unexpected_error            = 9
            OTHERS                      = 10.
      CATCH cx_sy_dyn_call_param_not_found.
        ii_package->set_permissions_changeable(
          EXPORTING
            i_changeable                = iv_lock
          EXCEPTIONS
            object_already_changeable   = 1
            object_already_unlocked     = 2
            object_locked_by_other_user = 3
            object_modified             = 4
            object_just_created         = 5
            object_deleted              = 6
            permission_failure          = 7
            object_invalid              = 8
            unexpected_error            = 9
            OTHERS                      = 10 ).
    ENDTRY.
    IF ( sy-subrc = 1 AND iv_lock = abap_true ) OR ( sy-subrc = 2 AND iv_lock = abap_false ).
      " There's no getter to find out beforehand...
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD unlock_and_raise_error.

    DATA ls_msg TYPE bal_s_msg.

    " Remember message since unlock overwrites it (for example with XT465)
    MOVE-CORRESPONDING sy TO ls_msg.

    set_lock( ii_package = ii_package
              iv_lock    = abap_false ).

    Lcx_abapgit_exception=>raise_t100(
      iv_msgid = ls_msg-msgid
      iv_msgno = ls_msg-msgno
      iv_msgv1 = ls_msg-msgv1
      iv_msgv2 = ls_msg-msgv2
      iv_msgv3 = ls_msg-msgv3
      iv_msgv4 = ls_msg-msgv4 ).

  ENDMETHOD.
  METHOD update_pinf_usages.
    DATA: lt_current_permissions TYPE tpak_permission_to_use_list,
          li_usage               TYPE REF TO if_package_permission_to_use,
          ls_data_sign           TYPE scomppsign,
          ls_add_permission_data TYPE pkgpermdat,
          lt_handled             TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    FIELD-SYMBOLS: <ls_usage_data> LIKE LINE OF it_usage_data.

    " Get the current permissions
    ii_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_current_permissions
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ls_data_sign-err_sever = abap_true.

    " New permissions
    LOOP AT it_usage_data ASSIGNING <ls_usage_data>.
      READ TABLE lt_current_permissions
           WITH KEY table_line->package_interface_name = <ls_usage_data>-intf_name
           INTO li_usage.

      IF sy-subrc = 0 AND li_usage IS BOUND.
        INSERT sy-tabix INTO TABLE lt_handled.

        " Permission already exists, update attributes
        li_usage->set_all_attributes(
          EXPORTING
            i_permission_data     = <ls_usage_data>
            i_data_sign           = ls_data_sign
          EXCEPTIONS
            object_not_changeable = 1
            object_invalid        = 2
            intern_err            = 3
            OTHERS                = 4 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      ELSE.
        " Permission does not exist yet, add it
        MOVE-CORRESPONDING <ls_usage_data> TO ls_add_permission_data.
        ii_package->add_permission_to_use(
          EXPORTING
            i_pkg_permission_data   = ls_add_permission_data
          EXCEPTIONS
            object_not_changeable   = 1
            object_access_error     = 2
            object_already_existing = 3
            object_invalid          = 4
            unexpected_error        = 5
            OTHERS                  = 6 ).
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.

      ENDIF.

      FREE li_usage.
    ENDLOOP.

    " Delete missing usages
    LOOP AT lt_current_permissions INTO li_usage.
      READ TABLE lt_handled WITH TABLE KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      li_usage->delete(
        EXCEPTIONS
          object_not_changeable = 1
          object_invalid        = 2
*          deletion_not_allowed  = 3 downport, does not exist in 7.30
          intern_err            = 4
          OTHERS                = 5 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA li_package TYPE REF TO if_package.

    li_package = get_package( ).
    IF li_package IS BOUND.
      rv_user = li_package->changed_by.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_package TYPE REF TO if_package,
          lv_package TYPE devclass.

    " Package deletion is a bit tricky. A package can only be deleted if there are no objects
    " contained in it. This includes subpackages, so first the leaf packages need to be deleted.
    " Unfortunately deleted objects that are still contained in an unreleased transport request
    " also count towards the contained objects counter.
    " -> Currently we delete only empty packages
    "
    " If objects are deleted, the TADIR entry is deleted when the transport request is released.
    " So before we can delete the package, the transport which deletes the objects
    " in the package has to be released.

    lv_package = ms_item-obj_name.

    " Remove remaining OTR entries
    Lcl_abapgit_sotr_handler=>delete_sotr_package( iv_package ).

    remove_obsolete_tadir( lv_package ).

    IF is_empty( lv_package ) = abap_true.

      li_package = load_package( lv_package ).

      IF li_package IS NOT BOUND.
        RETURN.
      ENDIF.

      IF lv_package(1) = '$'.
        Lcl_abapgit_persist_packages=>get_instance( )->modify( lv_package ).
      ENDIF.

      set_lock( ii_package = li_package
                iv_lock    = abap_true ).

      TRY.
          CALL METHOD li_package->('DELETE')
            EXPORTING
              i_suppress_dialog     = abap_true  " Parameter missing in 702
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5.

        CATCH cx_sy_dyn_call_param_not_found.

          li_package->delete(
            EXCEPTIONS
              object_not_empty      = 1
              object_not_changeable = 2
              object_invalid        = 3
              intern_err            = 4
              OTHERS                = 5 ).

      ENDTRY.

      IF sy-subrc <> 0.
        unlock_and_raise_error( li_package ).
      ENDIF.

      TRY.
          CALL METHOD li_package->('SAVE')
            EXPORTING
              i_suppress_dialog     = abap_true
            EXCEPTIONS
              object_invalid        = 1
              object_not_changeable = 2
              cancelled_in_corr     = 3
              permission_failure    = 4
              unexpected_error      = 5
              intern_err            = 6
              OTHERS                = 7.

        CATCH cx_sy_dyn_call_param_not_found.

          li_package->save(
            EXCEPTIONS
              object_invalid        = 1
              object_not_changeable = 2
              cancelled_in_corr     = 3
              permission_failure    = 4
              unexpected_error      = 5
              intern_err            = 6
              OTHERS                = 7 ).

      ENDTRY.

      IF sy-subrc <> 0.
        unlock_and_raise_error( li_package ).
      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_package      TYPE REF TO if_package,
          ls_package_data TYPE scompkdtln,
          ls_data_sign    TYPE scompksign,
          lt_usage_data   TYPE scomppdata,
          ls_save_sign    TYPE paksavsign.

    FIELD-SYMBOLS: <ls_usage_data> TYPE scomppdtln.
    FIELD-SYMBOLS: <lg_field> TYPE any.

    mv_local_devclass = iv_package.

    io_xml->read(
      EXPORTING
        iv_name = 'DEVC'
      CHANGING
        cg_data = ls_package_data ).

    IF mv_local_devclass(1) = '$'.
      IF ls_package_data-mainpack = 'X'.
        Lcx_abapgit_exception=>raise( |Main package { iv_package } cannot be used in local package| ).
      ELSEIF ls_package_data-mainpack = 'S'.
        Lcx_abapgit_exception=>raise( |Structure package { iv_package } cannot be used in local package| ).
      ENDIF.
    ENDIF.

    li_package = get_package( ).

    " Swap out repository package name with the local installation package name
    ls_package_data-devclass = mv_local_devclass.
    IF li_package IS BOUND.
      ls_package_data-pdevclass = li_package->transport_layer.
    ENDIF.

    " For local packages store application component
    IF ls_package_data-devclass(1) = '$'.
      Lcl_abapgit_persist_packages=>get_instance( )->modify(
        iv_package    = ls_package_data-devclass
        iv_component  = ls_package_data-component
        iv_comp_posid = ls_package_data-comp_posid ).
    ENDIF.

    " Parent package is not changed. Assume the folder logic already created the package and set
    " the hierarchy before.
    CLEAR ls_package_data-parentcl.

    ASSIGN COMPONENT 'PACKKIND' OF STRUCTURE ls_package_data TO <lg_field>.
    IF sy-subrc = 0.
      set_abap_language_version( CHANGING cv_abap_language_version = <lg_field> ).
    ENDIF.
    ASSIGN COMPONENT 'PACKKIND' OF STRUCTURE ls_data_sign TO <lg_field>.
    IF sy-subrc = 0.
      <lg_field> = abap_true.
    ENDIF.

* Fields not set:
* korrflag
* dlvunit
* parentcl
* cli_check
* intprefx
    ls_data_sign-ctext            = abap_true.
    ls_data_sign-as4user          = abap_true.
    ls_data_sign-pdevclass        = abap_true.
    ls_data_sign-comp_posid       = abap_true.
    ls_data_sign-component        = abap_true.
    ls_data_sign-perminher        = abap_true.
    ls_data_sign-packtype         = abap_true.
    ls_data_sign-restricted       = abap_true.
    ls_data_sign-mainpack         = abap_true.
    ls_data_sign-srv_check        = abap_true.
    ls_data_sign-ext_alias        = abap_true.
    ls_data_sign-project_guid     = abap_true.
    ls_data_sign-project_id       = abap_true.
    ls_data_sign-project_passdown = abap_true.

    IF ls_package_data-ctext IS INITIAL.
      ls_package_data-ctext = mv_local_devclass.
    ENDIF.
    IF ls_package_data-dlvunit IS INITIAL.
      ls_package_data-dlvunit = 'HOME'.
    ENDIF.

    ls_package_data-as4user = sy-uname.

    IF li_package IS BOUND.
      " Package already exists, change it
      set_lock( ii_package = li_package
                iv_lock    = abap_true ).

      li_package->set_all_attributes(
        EXPORTING
          i_package_data             = ls_package_data
          i_data_sign                = ls_data_sign
        EXCEPTIONS
          object_not_changeable      = 1
          object_deleted             = 2
          object_invalid             = 3
          short_text_missing         = 4
          author_not_existing        = 5
          local_package              = 6
          software_component_invalid = 7
          layer_invalid              = 8
          korrflag_invalid           = 9
          component_not_existing     = 10
          component_missing          = 11
          authorize_failure          = 12
          prefix_in_use              = 13
          unexpected_error           = 14
          intern_err                 = 15
*          wrong_mainpack_value       = 16  downport, does not exist in 7.30
*          superpackage_invalid       = 17  downport, does not exist in 7.30
          OTHERS                     = 18 ).
      IF sy-subrc <> 0.
        unlock_and_raise_error( li_package ).
      ENDIF.

    ELSE.
      " Package does not exist yet, create it
      " This shouldn't really happen, because the folder logic initially creates the packages.
      cl_package_factory=>create_new_package(
        IMPORTING
          e_package                  = li_package
        CHANGING
          c_package_data             = ls_package_data
        EXCEPTIONS
          object_already_existing    = 1
          object_just_created        = 2
          not_authorized             = 3
          wrong_name_prefix          = 4
          undefined_name             = 5
          reserved_local_name        = 6
          invalid_package_name       = 7
          short_text_missing         = 8
          software_component_invalid = 9
          layer_invalid              = 10
          author_not_existing        = 11
          component_not_existing     = 12
          component_missing          = 13
          prefix_in_use              = 14
          unexpected_error           = 15
          intern_err                 = 16
          no_access                  = 17
*          invalid_translation_depth  = 18 downport, does not exist in 7.30
*          wrong_mainpack_value       = 19 downport, does not exist in 7.30
*          superpackage_invalid       = 20 downport, does not exist in 7.30
*          error_in_cts_checks        = 21 downport, does not exist in 7.31
          OTHERS                     = 22 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

    " Load package interface usages
    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PERMISSION'
          CHANGING
            cg_data = lt_usage_data ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
        " No permissions saved
    ENDTRY.

    LOOP AT lt_usage_data ASSIGNING <ls_usage_data>.
      <ls_usage_data>-client_pak = mv_local_devclass.
    ENDLOOP.

    update_pinf_usages( ii_package    = li_package
                        it_usage_data = lt_usage_data ).

    ls_save_sign-pack   = abap_true.
    ls_save_sign-permis = abap_true.
    ls_save_sign-elems  = abap_true.
    ls_save_sign-interf = abap_true.

    li_package->save_generic(
      EXPORTING
        i_save_sign           = ls_save_sign
        i_transport_request   = iv_transport
        i_suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled_in_corr     = 1
        permission_failure    = 2
        object_not_changeable = 3
        object_invalid        = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      unlock_and_raise_error( li_package ).
    ENDIF.

    set_lock( ii_package = li_package
              iv_lock    = abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    " Check remote package if deserialize has not been called before this
    IF mv_local_devclass IS INITIAL.
      rv_bool = abap_false.
    ELSE.
      cl_package_helper=>check_package_existence(
        EXPORTING
          i_package_name          = mv_local_devclass
        IMPORTING
          e_package_exists        = rv_bool
        EXCEPTIONS
          intern_err              = 1
          OTHERS                  = 2 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'DV' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    DATA: ls_package_data TYPE scompkdtln,
          ls_package_comp TYPE Lcl_abapgit_persist_packages=>ty_package,
          li_package      TYPE REF TO if_package,
          lt_intf_usages  TYPE tpak_permission_to_use_list,
          lt_usage_data   TYPE scomppdata,
          ls_usage_data   TYPE scomppdtln,
          li_usage        TYPE REF TO if_package_permission_to_use.

    FIELD-SYMBOLS: <lg_field> TYPE any.


    li_package = get_package( ).
    IF li_package IS NOT BOUND.
      Lcx_abapgit_exception=>raise( |Could not find package to serialize.| ).
    ENDIF.

    li_package->get_all_attributes(
      IMPORTING
        e_package_data  = ls_package_data
      EXCEPTIONS
        object_invalid  = 1
        package_deleted = 2
        intern_err      = 3
        OTHERS          = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    " For local packages get application component
    IF is_local( ls_package_data-devclass ) = abap_true.
      ls_package_comp = Lcl_abapgit_persist_packages=>get_instance( )->read( ls_package_data-devclass ).
      ls_package_data-component  = ls_package_comp-component.
      ls_package_data-comp_posid = ls_package_comp-comp_posid.
    ENDIF.

    CLEAR: ls_package_data-devclass,
           ls_package_data-parentcl.

    " Clear administrative data to prevent diffs
    CLEAR: ls_package_data-created_by,
           ls_package_data-created_on,
           ls_package_data-changed_by,
           ls_package_data-changed_on,
           ls_package_data-as4user.

    " Clear text descriptions that might be localized
    CLEAR: ls_package_data-comp_text,
           ls_package_data-dlvu_text,
           ls_package_data-layer_text.

    " Clear obsolete fields
    CLEAR: ls_package_data-intfprefx,
           ls_package_data-cli_check.

    ASSIGN COMPONENT 'TRANSLATION_DEPTH_TEXT'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    ASSIGN COMPONENT 'TRANSLATION_GRAPH_DEPTH_TEXT'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    " Clear things related to local installation package
    CLEAR: ls_package_data-namespace,
           ls_package_data-dlvunit,
           ls_package_data-tpclass,
           ls_package_data-pdevclass.

    " Not usable on customer systems
    ASSIGN COMPONENT 'TRANSLATION_DEPTH'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    ASSIGN COMPONENT 'TRANSLATION_GRAPH_DEPTH'
           OF STRUCTURE ls_package_data
           TO <lg_field>.
    IF sy-subrc = 0.
      CLEAR: <lg_field>.
    ENDIF.

    CLEAR: ls_package_data-korrflag.

    ASSIGN COMPONENT 'PACKKIND' OF STRUCTURE ls_package_data TO <lg_field>.
    IF sy-subrc = 0.
      clear_abap_language_version( CHANGING cv_abap_language_version = <lg_field> ).
    ENDIF.

    io_xml->add( iv_name = 'DEVC'
                 ig_data = ls_package_data ).

    " Save package interface usages
    li_package->get_permissions_to_use(
      IMPORTING
        e_permissions    = lt_intf_usages
      EXCEPTIONS
        object_invalid   = 1
        unexpected_error = 2
        OTHERS           = 3 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_intf_usages INTO li_usage.
      li_usage->get_all_attributes(
        IMPORTING
          e_permission_data = ls_usage_data
        EXCEPTIONS
          object_invalid    = 1
          intern_err        = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      CLEAR: ls_usage_data-pack_name, ls_usage_data-client_pak.

      APPEND ls_usage_data TO lt_usage_data.
    ENDLOOP.

    IF lt_usage_data IS NOT INITIAL.
      io_xml->add( iv_name = 'PERMISSION'
                   ig_data = lt_usage_data ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.

    IF iv_filename <> Lcl_abapgit_filename_logic=>c_package_file.
      Lcx_abapgit_exception=>raise( |Unexpected filename for package { cs_item-obj_name }| ).
    ENDIF.

    " Try to get a unique package name for DEVC by using the path
    cs_item-obj_name = Lcl_abapgit_folder_logic=>get_instance( )->path_to_package(
      iv_top                  = iv_package
      io_dot                  = io_dot
      iv_create_if_not_exists = abap_false
      iv_path                 = iv_path ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.

    " Packages have a fixed filename so that the repository can be installed to a different
    " package(-hierarchy) on the client and not show up as a different package in the repo.
    cv_filename = Lcl_abapgit_filename_logic=>c_package_file.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DEVC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_DIAL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_dial=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_dial=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_DIAL implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSDIA'.
    ls_bcdata-dynpro   = '1010'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'DIAPAR-DNAM'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'RS38L-PARM'.
    ls_bcdata-fval     = abap_true.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=DELF'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-program  = 'SAPLSPO1'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=YES'.
    APPEND ls_bcdata TO lt_bcdata.

    ls_bcdata-program  = 'SAPMSDIA'.
    ls_bcdata-dynpro   = '1010'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=BACK'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode      = 'SE35'
      it_bdcdata    = lt_bcdata
      iv_new_window = abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_dialog_module TYPE ty_dialog_module.

    io_xml->read(
      EXPORTING
        iv_name = 'DIAL'
      CHANGING
        cg_data = ls_dialog_module ).

    CALL FUNCTION 'RS_DIALOG_CREATE'
      EXPORTING
        dialogname            = ls_dialog_module-tdct-dnam
        dynpronumber          = ls_dialog_module-tdct-dynr
        programname           = ls_dialog_module-tdct-prog
        suppress_corr_check   = abap_false
*     It seems that dia_par parameter doesn't do anything, but we can't omit it
*     Parameters are inserted below
      TABLES
        dia_par               = ls_dialog_module-dia_pars
      EXCEPTIONS
        dialog_already_exists = 1
        invalid_name          = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error deserializing dialogmodule { ms_item-obj_name }| ).
    ENDIF.

    " It seems that there's no API for diapar, therefore we manipulate it directly
    INSERT diapar FROM TABLE ls_dialog_module-dia_pars.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_tdct TYPE tdct.

    ls_tdct = _read_tdct( ).

    rv_bool = boolc( ls_tdct IS NOT INITIAL ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lv_objectname TYPE tdct-dnam.

    lv_objectname = ms_item-obj_name.

    CALL FUNCTION 'RS_DIALOG_SHOW'
      EXPORTING
        objectname       = lv_objectname
        type             = 'VW'
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA ls_dialog_module TYPE ty_dialog_module.

    ls_dialog_module-tdct = _read_tdct( ).

    SELECT * FROM diapar
      INTO TABLE ls_dialog_module-dia_pars
      WHERE dnam = ls_dialog_module-tdct-dnam
      ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'DIAL'
                 ig_data = ls_dialog_module ).

  ENDMETHOD.
  METHOD _read_tdct.

    DATA: lv_dnam TYPE tdct-dnam.

    lv_dnam = ms_item-obj_name.

    SELECT SINGLE * FROM tdct
           INTO rs_tdct
           WHERE dnam = lv_dnam.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_DIAL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ECATT_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ecatt_superccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ecatt_superccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_ecatt_superccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KW4GSFY DEFINITION DEFERRED.

*CLASS zcl_abapgit_object_ecatt_super DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW4GSFY.




class LCL_ABAPGIT_OBJECT_ECATT_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD clear_element_collection.

    DATA:
      lo_node_collection TYPE REF TO if_ixml_node_collection,
      lo_node            TYPE REF TO if_ixml_node,
      lv_index           TYPE i.

    lo_node_collection = ci_document->get_elements_by_tag_name( iv_name ).

    lv_index = 0.
    WHILE lv_index < lo_node_collection->get_length( ).
      lo_node = lo_node_collection->get_item( lv_index ).
      lo_node->set_value( '' ).
      lv_index = lv_index + 1.
    ENDWHILE.

  ENDMETHOD.
  METHOD clear_attributes.

    DATA: li_element     TYPE REF TO if_ixml_element,
          lv_object_type TYPE etobj_type.

    lv_object_type = get_object_type( ).

    li_element = ci_document->find_from_name( |{ lv_object_type }| ).
    li_element->remove_attribute( |SAPRL| ).
    li_element->remove_attribute( |DOWNLOADDATE| ).
    li_element->remove_attribute( |DOWNLOADTIME| ).

  ENDMETHOD.
  METHOD clear_element.

    DATA: li_element TYPE REF TO if_ixml_element.

    li_element = ci_document->find_from_name( iv_name ).

    IF li_element IS BOUND.
      li_element->set_value( || ).
    ENDIF.

  ENDMETHOD.
  METHOD clear_elements.

    clear_element( EXPORTING iv_name     = |FUSER|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |FDATE|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |LUSER|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |LDATE|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |LTIME|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |TWB_RESP|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |DEVCLASS|
                   CHANGING  ci_document = ci_document ).

    clear_element( EXPORTING iv_name     = |TADIR_RESP|
                   CHANGING  ci_document = ci_document ).

    " Clearing just VAR_EXT_PATH will lead to diffs in batch
    clear_element( EXPORTING iv_name     = |ETVAR_EXT|
                   CHANGING  ci_document = ci_document ).

    " SORTLNR is part of ETPAR_VARI and causing diffs
    " We can clear it since it's automatically filled during deserialize
    clear_element_collection( EXPORTING iv_name     = |SORTLNR|
                              CHANGING  ci_document = ci_document ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_object_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD deserialize_version.

    DATA: ls_object   TYPE etmobjects,
          lo_upload   TYPE REF TO cl_apl_ecatt_upload,
          li_upload   TYPE REF TO Lif_abapgit_ecatt_upload,
          lv_xml      TYPE xstring,
          li_document TYPE REF TO if_ixml_document,
          lv_version  TYPE string,
          lx_error    TYPE REF TO cx_ecatt.

    lv_version = get_version_from_node( ii_version_node ).

    IF lv_version IS INITIAL.
      RETURN.
    ENDIF.

    lo_upload  = get_upload( ).
    li_upload ?= lo_upload.

    li_document = cl_ixml=>create( )->create_document( ).
    li_document->append_child( ii_version_node->get_first_child( ) ).

    lv_xml = cl_ixml_80_20=>render_to_xstring( li_document ).

    li_upload->set_stream_for_upload( lv_xml ).

    ls_object-d_obj_name  = mv_object_name.
    ls_object-s_obj_type  = get_object_type( ).
    ls_object-d_devclass  = iv_package.
    ls_object-d_obj_ver   = lv_version.
    ls_object-d_overwrite = abap_true.

    TRY.
        lo_upload->upload( CHANGING ch_object = ls_object ).

      CATCH cx_ecatt INTO lx_error.
        Lcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_changed_by_user.

    rv_changed_by_user = ii_document->find_from_name( 'LUSER' )->get_value( ).

  ENDMETHOD.
  METHOD get_changed_date.

    DATA: lv_changed_date_external TYPE string.

    lv_changed_date_external = ii_document->find_from_name( 'LDATE' )->get_value( ).

    REPLACE ALL OCCURRENCES OF '-' IN lv_changed_date_external WITH ''.
    rv_changed_date = lv_changed_date_external.

  ENDMETHOD.
  METHOD get_changed_time.

    DATA: lv_changed_time_external TYPE string.

    lv_changed_time_external = ii_document->find_from_name( 'LTIME' )->get_value( ).

    REPLACE ALL OCCURRENCES OF ':' IN lv_changed_time_external WITH ''.
    rv_changed_time = lv_changed_time_external.

  ENDMETHOD.
  METHOD get_change_information.

    DATA: li_document    TYPE REF TO if_ixml_document,
          lv_xml         TYPE xstring,
          lo_download    TYPE REF TO cl_apl_ecatt_download,
          lv_object_type TYPE etobj_type.

    lo_download = get_download( ).

    lv_object_type = get_object_type( ).

    lv_xml = Lcl_abapgit_ecatt_helper=>build_xml_of_object(
                 iv_object_name    = mv_object_name
                 iv_object_version = is_version_info-version
                 iv_object_type    = lv_object_type
                 io_download       = lo_download ).

    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml ).

    rs_change_information-ldate = get_changed_date( li_document ).
    rs_change_information-ltime = get_changed_time( li_document ).
    rs_change_information-luser = get_changed_by_user( li_document ).

  ENDMETHOD.
  METHOD get_version_from_node.

    TRY.
        rv_version = ii_node->get_first_child(
                           )->get_first_child(
                           )->get_first_child(
                           )->get_first_child(
                           )->get_value( ).

      CATCH cx_sy_ref_is_initial.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD is_change_more_recent_than.

    IF is_currently_changed-ldate > is_last_changed-ldate
      OR (     is_currently_changed-ldate = is_last_changed-ldate
           AND is_currently_changed-ltime > is_last_changed-ltime ).

      rv_is_change_more_recent = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD serialize_version.

    DATA: li_document    TYPE REF TO if_ixml_document,
          lv_xml         TYPE xstring,
          li_node        TYPE REF TO if_ixml_element,
          lo_download    TYPE REF TO cl_apl_ecatt_download,
          lv_object_type TYPE etobj_type.

    lo_download = get_download( ).

    lv_object_type = get_object_type( ).

    lv_xml = Lcl_abapgit_ecatt_helper=>build_xml_of_object(
                 iv_object_name    = mv_object_name
                 iv_object_version = iv_version
                 iv_object_type    = lv_object_type
                 io_download       = lo_download ).

    IF lv_xml IS INITIAL.
      Lcx_abapgit_exception=>raise( |ECATT, empty xml, { mv_object_name }| ).
    ENDIF.

    li_document = cl_ixml_80_20=>parse_to_document( stream_xstring = lv_xml ).

    clear_attributes( CHANGING ci_document = li_document ).

    clear_elements( CHANGING ci_document = li_document ).

    li_node = li_document->create_element( c_name-version ).
    li_node->append_child( li_document->get_root_element( ) ).

    ci_node->append_child( li_node ).

  ENDMETHOD.
  METHOD serialize_versions.

    DATA: li_versions_node TYPE REF TO if_ixml_element.
    FIELD-SYMBOLS: <ls_version_info> LIKE LINE OF it_version_info.

    li_versions_node = ci_document->create_element( c_name-versions ).

    IF lines( it_version_info ) > 0.

      LOOP AT it_version_info ASSIGNING <ls_version_info>.

        serialize_version(
          EXPORTING
            iv_version = <ls_version_info>-version
          CHANGING
            ci_node    = li_versions_node ).

      ENDLOOP.

    ELSE.

      serialize_version(
        EXPORTING
          iv_version = c_default_version
        CHANGING
          ci_node    = li_versions_node ).

    ENDIF.

    ci_document->append_child( li_versions_node ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_last_changed      TYPE ty_last_changed,
          ls_currently_changed TYPE ty_last_changed,
          lt_version_info      TYPE etversinfo_tabtype,
          lx_error             TYPE REF TO cx_static_check,
          lv_text              TYPE string,
          lv_object_type       TYPE etobj_type.

    FIELD-SYMBOLS: <ls_version_info> LIKE LINE OF lt_version_info.

    TRY.
        lv_object_type = get_object_type( ).

        cl_apl_ecatt_object=>get_version_info_object(
          EXPORTING
            im_name          = mv_object_name
            im_obj_type      = lv_object_type
          IMPORTING
            ex_version_info  = lt_version_info ).

        LOOP AT lt_version_info ASSIGNING <ls_version_info>.

          ls_currently_changed = get_change_information( <ls_version_info> ).

          IF is_change_more_recent_than( is_currently_changed = ls_currently_changed
                                         is_last_changed      = ls_last_changed ) = abap_true.
            ls_last_changed = ls_currently_changed.
          ENDIF.

        ENDLOOP.

      CATCH cx_static_check INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    IF ls_last_changed-luser IS NOT INITIAL.
      rv_user = ls_last_changed-luser.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lx_error       TYPE REF TO cx_ecatt_apl,
          lv_text        TYPE string,
          lv_object_type TYPE etobj_type.

    lv_object_type = get_object_type( ).

    TRY.
        cl_apl_ecatt_object=>delete_object( im_obj_type            = lv_object_type
                                            im_name                = mv_object_name
                                            " we have to supply a version, so let's use the default version
                                            " and delete them all
                                            im_version             = c_default_version
                                            im_delete_all_versions = abap_true ).

      CATCH cx_ecatt_apl INTO lx_error.
        lv_text = lx_error->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_document         TYPE REF TO if_ixml_document,
          li_versions         TYPE REF TO if_ixml_node_collection,
          li_version_iterator TYPE REF TO if_ixml_node_iterator,
          li_version_node     TYPE REF TO if_ixml_node.

    li_document = io_xml->get_raw( ).

    li_versions = li_document->get_elements_by_tag_name( depth = 0
                                                         name  = c_name-version ).

    li_version_iterator = li_versions->create_iterator( ).

    DO.
      li_version_node = li_version_iterator->get_next( ).

      IF li_version_node IS NOT BOUND.
        EXIT.
      ENDIF.

      deserialize_version( ii_version_node = li_version_node
                           iv_package      = iv_package ).

    ENDDO.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_object_type TYPE etobj_type.

    lv_object_type = get_object_type( ).

    TRY.
        rv_bool = cl_apl_ecatt_object=>existence_check_object( im_name               = mv_object_name
                                                               im_version            = c_default_version
                                                               im_obj_type           = lv_object_type
                                                               im_exists_any_version = abap_true ).

      CATCH cx_ecatt.
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE seqg3-garg.

    lv_object = ms_item-obj_name.
    OVERLAY lv_object WITH '                              '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = get_lock_object( )
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      lt_version_info TYPE etversinfo_tabtype,
      li_document     TYPE REF TO if_ixml_document,
      lx_error        TYPE REF TO cx_ecatt,
      lv_text         TYPE string,
      lv_object_type  TYPE etobj_type.

    lv_object_type = get_object_type( ).

    TRY.
        cl_apl_ecatt_object=>get_version_info_object(
          EXPORTING
            im_name         = mv_object_name
            im_obj_type     = lv_object_type
          IMPORTING
            ex_version_info = lt_version_info ).

        SORT lt_version_info BY version.

        li_document = cl_ixml=>create( )->create_document( ).

        serialize_versions(
          EXPORTING
            it_version_info = lt_version_info
          CHANGING
            ci_document     = li_document ).

        io_xml->set_raw( li_document->get_root_element( ) ).

      CATCH cx_ecatt INTO lx_error.
        lv_text = lx_error->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ECATT_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_EEEC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_eeec=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_eeec=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_EEEC implementation.
*"* method's implementations
*include methods.
  METHOD get_object_handler.

    DATA lx_error TYPE REF TO cx_root.

    ro_object_handler = super->get_object_handler( ).

    IF ro_object_handler IS NOT BOUND.
      TRY.
          CREATE OBJECT ro_object_handler TYPE ('/IWXBE/CL_EEEC_AFF_OBJECTHANDL').
        CATCH cx_root INTO lx_error.
          Lcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                        ix_previous = lx_error ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_data             TYPE REF TO data,
          lo_registry_adapter TYPE REF TO object,
          lv_object_key       TYPE seu_objkey,
          lx_error            TYPE REF TO cx_root.

    FIELD-SYMBOLS: <ls_consumer>   TYPE any,
                   <lv_changed_by> TYPE any.

    TRY.
        CREATE OBJECT lo_registry_adapter TYPE ('/IWXBE/CL_EEEC_REG_ADAPTER').
        CREATE DATA lr_data TYPE ('/IWXBE/IF_REGISTRY_TYPES=>TY_S_CONSUMER').
        ASSIGN lr_data->* TO <ls_consumer>.

        lv_object_key = ms_item-obj_name.

        TRY.
            CALL METHOD lo_registry_adapter->('/IWXBE/IF_EEEC_REG_ADAPTER_WB~GET_METADATA')
              EXPORTING
                iv_object_key = lv_object_key
                iv_state      = 'I'
              RECEIVING
                rs_consumer   = <ls_consumer>.

          CATCH cx_root.
            CALL METHOD lo_registry_adapter->('/IWXBE/IF_EEEC_REG_ADAPTER_WB~GET_METADATA')
              EXPORTING
                iv_object_key = lv_object_key
                iv_state      = 'A'
              RECEIVING
                rs_consumer   = <ls_consumer>.
        ENDTRY.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <ls_consumer> TO <lv_changed_by>.
        rv_user = <lv_changed_by>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise( iv_text     = lx_error->get_text( )
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_EEEC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FTGL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ftgl=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ftgl=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FTGL implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN
      COMPONENT iv_fieldname
      OF STRUCTURE cg_header
      TO <lg_field>.
    ASSERT sy-subrc = 0.

    CLEAR: <lg_field>.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_toggle_id = ms_item-obj_name.

    TRY.
        CREATE DATA mr_toggle TYPE ('FTGL_S_WB_FEATURE_TOGGLE').
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |FTGL not supported in your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changedby FROM ('FTGL_ID') INTO rv_user
      WHERE feature_id = ms_item-obj_name AND version = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lv_return_code TYPE i.

    CALL METHOD ('CL_FEATURE_TOGGLE_OBJECT')=>delete
      EXPORTING
        iv_toggle_id = mv_toggle_id
      RECEIVING
        rv_rc        = lv_return_code.

    IF lv_return_code <> 0.
      Lcx_abapgit_exception=>raise( |Cannot delete feature toggle { mv_toggle_id }. |
                                 && |Error { sy-subrc } from cl_feature_toggle_object=>delete| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lo_toggle TYPE REF TO object,
      lx_error  TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lg_toggle> TYPE data.

    ASSIGN mr_toggle->* TO <lg_toggle>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'FTGL'
      CHANGING
        cg_data = <lg_toggle> ).

    TRY.
        CALL METHOD ('CL_FEATURE_TOGGLE_OBJECT')=>create_toggle_by_content
          EXPORTING
            is_content = <lg_toggle>
          RECEIVING
            ro_toggle  = lo_toggle.

        CALL METHOD lo_toggle->('SAVE').

        tadir_insert( iv_package ).
        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD ('CL_FEATURE_TOGGLE')=>is_defined
      EXPORTING
        iv_toggle_id = mv_toggle_id
      RECEIVING
        rc_exists    = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_FTGL'
                                            iv_argument    = |{ mv_toggle_id }*| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      lx_error  TYPE REF TO cx_root,
      lo_toggle TYPE REF TO object.

    FIELD-SYMBOLS: <lg_toggle> TYPE data.

    ASSIGN mr_toggle->* TO <lg_toggle>.
    ASSERT sy-subrc = 0.

    TRY.
        CALL METHOD ('CL_FEATURE_TOGGLE_OBJECT')=>create_toggle_by_id
          EXPORTING
            iv_toggle_id = mv_toggle_id
          RECEIVING
            ro_toggle    = lo_toggle.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    CALL METHOD lo_toggle->('GET_CONTENT')
      RECEIVING
        rs_content = <lg_toggle>.

    clear_field( EXPORTING iv_fieldname = 'HEADER-OWNER'        CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CREATED_DATE' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CREATED_TIME' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CHANGEDBY   ' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CHANGED_DATE' CHANGING cg_header = <lg_toggle> ).
    clear_field( EXPORTING iv_fieldname = 'HEADER-CHANGED_TIME' CHANGING cg_header = <lg_toggle> ).

    io_xml->add(
        iv_name = 'FTGL'
        ig_data = <lg_toggle> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FTGL implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_FUGR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_fugr=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_fugr=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_FUGR implementation.
*"* method's implementations
*include methods.
  METHOD check_rfc_parameters.

* function module RS_FUNCTIONMODULE_INSERT does the same deep down, but the right error
* message is not returned to the user, this is a workaround to give a proper error
* message to the user

    DATA: ls_parameter TYPE rsfbpara,
          lt_fupa      TYPE rsfb_param,
          ls_fupa      LIKE LINE OF lt_fupa.


    IF is_function-remote_call = 'R'.
      cl_fb_parameter_conversion=>convert_parameter_old_to_fupa(
        EXPORTING
          functionname = is_function-funcname
          import       = is_function-import
          export       = is_function-export
          change       = is_function-changing
          tables       = is_function-tables
          except       = is_function-exception
        IMPORTING
          fupararef    = lt_fupa ).

      LOOP AT lt_fupa INTO ls_fupa WHERE paramtype = 'I' OR paramtype = 'E' OR paramtype = 'C' OR paramtype = 'T'.
        cl_fb_parameter_conversion=>convert_intern_to_extern(
          EXPORTING
            parameter_db  = ls_fupa
          IMPORTING
            parameter_vis = ls_parameter ).

        CALL FUNCTION 'RS_FB_CHECK_PARAMETER_REMOTE'
          EXPORTING
            parameter             = ls_parameter
            basxml_enabled        = is_function-remote_basxml
          EXCEPTIONS
            not_remote_compatible = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          Lcx_abapgit_exception=>raise_t100( ).
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
  METHOD deserialize_functions.

    DATA: lv_include   TYPE rs38l-include,
          lv_area      TYPE rs38l-area,
          lv_group     TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lt_source    TYPE TABLE OF abaptxt255,
          lv_msg       TYPE string,
          lx_error     TYPE REF TO Lcx_abapgit_exception.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF it_functions.

    LOOP AT it_functions ASSIGNING <ls_func>.

      lt_source = Lif_abapgit_object~mo_files->read_abap( iv_extra = <ls_func>-funcname ).

      lv_area = ms_item-obj_name.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING
          complete_area = lv_area
        IMPORTING
          namespace     = lv_namespace
          group         = lv_group
        EXCEPTIONS
          OTHERS        = 12.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        ii_log->add_error( iv_msg  = |Function module { <ls_func>-funcname }: { lv_msg }|
                           is_item = ms_item ).
        CONTINUE. "with next function module
      ENDIF.

      IF Lcl_abapgit_factory=>get_function_module( )->function_exists( <ls_func>-funcname ) = abap_true.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_func>-funcname
            suppress_success_message = abap_true
          EXCEPTIONS
            error_message            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
          ii_log->add_error( iv_msg = |Function module { <ls_func>-funcname }: { lv_msg }|
                             is_item = ms_item ).
          CONTINUE. "with next function module
        ENDIF.
      ENDIF.

      TRY.
          check_rfc_parameters( <ls_func> ).
        CATCH Lcx_abapgit_exception INTO lx_error.
          ii_log->add_error(
            iv_msg  = |Function module { <ls_func>-funcname }: { lx_error->get_text( ) }|
            is_item = ms_item ).
          CONTINUE. "with next function module
      ENDTRY.

      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = <ls_func>-funcname
          function_pool           = lv_group
          interface_global        = <ls_func>-global_flag
          remote_call             = <ls_func>-remote_call
          short_text              = <ls_func>-short_text
          update_task             = <ls_func>-update_task
          exception_class         = <ls_func>-exception_classes
          namespace               = lv_namespace
          remote_basxml_supported = <ls_func>-remote_basxml
          corrnum                 = iv_transport
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = <ls_func>-import
          export_parameter        = <ls_func>-export
          tables_parameter        = <ls_func>-tables
          changing_parameter      = <ls_func>-changing
          exception_list          = <ls_func>-exception
          parameter_docu          = <ls_func>-documentation
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_msg.
        ii_log->add_error( iv_msg = |Function module { <ls_func>-funcname }: { lv_msg }|
                           is_item = ms_item ).
        CONTINUE.  "with next function module
      ENDIF.

      Lcl_abapgit_factory=>get_sap_report( )->insert_report(
        iv_name    = lv_include
        iv_package = iv_package
        iv_version = iv_version
        it_source  = lt_source ).

      ii_log->add_success( iv_msg = |Function module { <ls_func>-funcname } imported|
                           is_item = ms_item ).
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_function_docs.

    FIELD-SYMBOLS <ls_func> LIKE LINE OF it_functions.

    Lcl_abapgit_factory=>get_longtexts( )->deserialize(
      iv_longtext_id   = c_longtext_id_prog
      iv_object_name   = iv_prog_name
      ii_xml           = ii_xml
      iv_main_language = mv_language ).

    LOOP AT it_functions ASSIGNING <ls_func>.
      Lcl_abapgit_factory=>get_longtexts( )->deserialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }|
        iv_longtext_id   = c_longtext_id_func
        iv_object_name   = <ls_func>-funcname
        ii_xml           = ii_xml
        iv_main_language = mv_language ).
      Lcl_abapgit_factory=>get_longtexts( )->deserialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }___EXC|
        iv_longtext_id   = c_longtext_id_func_exc
        iv_object_name   = <ls_func>-funcname
        ii_xml           = ii_xml
        iv_main_language = mv_language ).
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_includes.

    DATA: lo_xml       TYPE REF TO Lif_abapgit_xml_input,
          ls_progdir   TYPE Lif_abapgit_sap_report=>ty_progdir,
          lt_includes  TYPE ty_sobj_name_tt,
          lt_tpool     TYPE textpool_table,
          lt_tpool_ext TYPE Lif_abapgit_definitions=>ty_tpool_tt,
          lt_source    TYPE TABLE OF abaptxt255,
          lx_exc       TYPE REF TO Lcx_abapgit_exception.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    tadir_insert( iv_package ).

    ii_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      "ignore simple transformation includes (as long as they remain in existing repositories)
      IF strlen( <lv_include> ) = 33 AND <lv_include>+30(3) = 'XTI'.
        ii_log->add_warning( iv_msg = |Simple Transformation include { <lv_include> } ignored|
                             is_item = ms_item ).
        CONTINUE.
      ENDIF.

      TRY.
          lt_source = Lif_abapgit_object~mo_files->read_abap( iv_extra = <lv_include> ).

          lo_xml = Lif_abapgit_object~mo_files->read_xml( <lv_include> ).

          lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                        CHANGING cg_data = ls_progdir ).

          set_abap_language_version( CHANGING cv_abap_language_version = ls_progdir-uccheck ).

          lo_xml->read( EXPORTING iv_name = 'TPOOL'
                        CHANGING cg_data = lt_tpool_ext ).
          lt_tpool = read_tpool( lt_tpool_ext ).

          deserialize_program( is_progdir = ls_progdir
                               it_source  = lt_source
                               it_tpool   = lt_tpool
                               iv_package = iv_package ).

          deserialize_textpool( iv_program    = <lv_include>
                                it_tpool      = lt_tpool
                                iv_is_include = abap_true ).

          ii_log->add_success( iv_msg = |Include { ls_progdir-name } imported|
                               is_item = ms_item ).

        CATCH Lcx_abapgit_exception INTO lx_exc.
          ii_log->add_exception( ix_exc = lx_exc
                                 is_item = ms_item ).
          CONTINUE.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_texts.
    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.
    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = iv_prog_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.
  ENDMETHOD.
  METHOD deserialize_xml.

    DATA: lv_complete  TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_areat     TYPE tlibt-areat,
          lv_stext     TYPE tftit-stext,
          lv_group     TYPE rs38l-area.

    lv_complete = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_complete
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    ii_xml->read( EXPORTING iv_name = 'AREAT'
                  CHANGING cg_data = lv_areat ).
    lv_stext = lv_areat.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_group
        short_text              = lv_stext
        namespace               = lv_namespace
        devclass                = iv_package
        unicode_checks          = iv_version
        corrnum                 = iv_transport
        suppress_corr_check     = abap_false
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.

    CASE sy-subrc.
      WHEN 0.
        " Everything is ok
      WHEN 1 OR 3.
        " If the function group exists we need to manually update the short text
        update_func_group_short_text( iv_group      = lv_group
                                      iv_short_text = lv_stext ).
      WHEN OTHERS.
        Lcx_abapgit_exception=>raise_t100( ).
    ENDCASE.

  ENDMETHOD.
  METHOD functions.

    DATA: lv_area TYPE rs38l-area.
    FIELD-SYMBOLS: <ls_functab> TYPE LINE OF ty_rs38l_incl_tt.

    lv_area = ms_item-obj_name.


    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* The result can also contain function which are lowercase.
    LOOP AT rt_functab ASSIGNING <ls_functab>.
      TRANSLATE <ls_functab> TO UPPER CASE.
    ENDLOOP.

    SORT rt_functab BY funcname ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_functab COMPARING funcname.

  ENDMETHOD.
  METHOD get_abap_version.

    DATA: lt_includes TYPE ty_sobj_name_tt,
          ls_progdir  TYPE Lif_abapgit_sap_report=>ty_progdir,
          lo_xml      TYPE REF TO Lif_abapgit_xml_input.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.

    ii_xml->read( EXPORTING iv_name = 'INCLUDES'
                  CHANGING cg_data = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      lo_xml = Lif_abapgit_object~mo_files->read_xml( <lv_include> ).

      lo_xml->read( EXPORTING iv_name = 'PROGDIR'
                    CHANGING cg_data = ls_progdir ).

      IF ls_progdir-uccheck IS INITIAL.
        CONTINUE.
      ELSEIF rv_abap_version IS INITIAL.
        rv_abap_version = ls_progdir-uccheck.
        CONTINUE.
      ELSEIF rv_abap_version <> ls_progdir-uccheck.
*** All includes need to have the same ABAP language version
        Lcx_abapgit_exception=>raise( 'different ABAP Language Versions' ).
      ENDIF.
    ENDLOOP.

    IF rv_abap_version IS INITIAL.
      set_abap_language_version( CHANGING cv_abap_language_version = rv_abap_version ).
    ENDIF.

  ENDMETHOD.
  METHOD includes.

    TYPES: BEGIN OF ty_reposrc,
             progname TYPE reposrc-progname,
           END OF ty_reposrc.

    DATA: lt_reposrc        TYPE STANDARD TABLE OF ty_reposrc WITH DEFAULT KEY,
          ls_reposrc        LIKE LINE OF lt_reposrc,
          lv_program        TYPE program,
          lv_maintviewname  LIKE LINE OF rt_includes,
          lv_offset_ns      TYPE i,
          lv_tabix          LIKE sy-tabix,
          lt_functab        TYPE ty_rs38l_incl_tt,
          lt_tadir_includes TYPE HASHED TABLE OF objname WITH UNIQUE KEY table_line.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF rt_includes,
                   <ls_func>    LIKE LINE OF lt_functab.


    IF lines( mt_includes_cache ) > 0.
      rt_includes = mt_includes_cache.
      RETURN.
    ENDIF.

    lv_program = main_name( ).
    lt_functab = functions( ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
      TABLES
        includetab   = rt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from RS_GET_ALL_INCLUDES' ).
    ENDIF.

    LOOP AT lt_functab ASSIGNING <ls_func>.
      DELETE TABLE rt_includes FROM <ls_func>-include.
    ENDLOOP.

* handle generated maintenance views
    IF ms_item-obj_name(1) <> '/'.
      "FGroup name does not contain a namespace
      lv_maintviewname = |L{ ms_item-obj_name }T00|.
    ELSE.
      "FGroup name contains a namespace
      lv_offset_ns = find( val = ms_item-obj_name+1
                           sub = '/' ).
      lv_offset_ns = lv_offset_ns + 2.
      lv_maintviewname = |{ ms_item-obj_name(lv_offset_ns) }L{ ms_item-obj_name+lv_offset_ns }T00|.
    ENDIF.

    READ TABLE rt_includes WITH KEY table_line = lv_maintviewname TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND lv_maintviewname TO rt_includes.
    ENDIF.

    SORT rt_includes.
    IF lines( rt_includes ) > 0.
      " check which includes have their own tadir entry
      " these includes might reside in a different package or might be shared between multiple function groups
      " or other programs and are hence no part of the to serialized FUGR object
      " they will be handled as individual objects when serializing their package
      " in addition, referenced XTI includes referencing (simple) transformations must be ignored
      SELECT obj_name
        INTO TABLE lt_tadir_includes
        FROM tadir
        FOR ALL ENTRIES IN rt_includes
        WHERE pgmid      = 'R3TR'
              AND object = 'PROG'
              AND obj_name = rt_includes-table_line.
      LOOP AT rt_includes ASSIGNING <lv_include>.
        " skip autogenerated includes from Table Maintenance Generator
        IF <lv_include> CP 'LSVIM*'.
          DELETE rt_includes INDEX sy-tabix.
          CONTINUE.
        ENDIF.
        READ TABLE lt_tadir_includes WITH KEY table_line = <lv_include> TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          DELETE rt_includes.
          CONTINUE.
        ENDIF.
        IF strlen( <lv_include> ) = 33 AND <lv_include>+30(3) = 'XTI'.
          "ignore referenced (simple) transformation includes
          DELETE rt_includes.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      IF lines( rt_includes ) > 0.
        SELECT progname FROM reposrc
          INTO TABLE lt_reposrc
          FOR ALL ENTRIES IN rt_includes
          WHERE progname = rt_includes-table_line
          AND r3state = 'A'.
      ENDIF.
      SORT lt_reposrc BY progname ASCENDING.
    ENDIF.

    LOOP AT rt_includes ASSIGNING <lv_include>.
      lv_tabix = sy-tabix.

* make sure the include exists
      READ TABLE lt_reposrc INTO ls_reposrc
        WITH KEY progname = <lv_include> BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE rt_includes INDEX lv_tabix.
        CONTINUE.
      ENDIF.

      "Make sure that the include does not belong to another function group
      IF is_part_of_other_fugr( <lv_include> ) = abap_true.
        DELETE rt_includes.
      ENDIF.
    ENDLOOP.

    APPEND lv_program TO rt_includes.
    SORT rt_includes.

    mt_includes_cache = rt_includes.

  ENDMETHOD.
  METHOD is_any_function_module_locked.

    DATA: lt_functions TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS: <ls_function> TYPE rs38l_incl.

    TRY.
        lt_functions = functions( ).
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_functions ASSIGNING <ls_function>.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESFUNCTION'
                                  iv_argument    = |{ <ls_function>-funcname }| ) = abap_true.
        rv_any_function_module_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_any_include_locked.

    DATA: lt_includes TYPE ty_sobj_name_tt.
    FIELD-SYMBOLS: <lv_include> TYPE sobj_name.

    TRY.
        lt_includes = includes( ).
      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_includes ASSIGNING <lv_include>.

      IF exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                  iv_argument    = |{ <lv_include> }| ) = abap_true.
        rv_is_any_include_locked = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD is_function_group_locked.
    rv_is_functions_group_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                                            iv_argument    = ms_item-obj_name
                                                            iv_prefix      = 'FG' ).
  ENDMETHOD.
  METHOD is_part_of_other_fugr.
    " make sure that the include belongs to the function group
    " like in LSEAPFAP Form TADIR_MAINTENANCE
    DATA ls_tadir TYPE tadir.
    DATA lv_namespace TYPE rs38l-namespace.
    DATA lv_area TYPE rs38l-area.
    DATA lv_include TYPE rs38l-include.

    rv_belongs_to_other_fugr = abap_false.
    IF iv_include(1) = 'L' OR iv_include+1 CS '/L'.
      lv_include = iv_include.
      ls_tadir-object = 'FUGR'.

      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        IMPORTING
          namespace = lv_namespace
          group     = lv_area
        CHANGING
          include   = lv_include
        EXCEPTIONS
          OTHERS    = 1.
      IF lv_area(1) = 'X'.    " "EXIT"-function-module
        ls_tadir-object = 'FUGS'.
      ENDIF.
      IF sy-subrc = 0.
        CONCATENATE lv_namespace lv_area INTO ls_tadir-obj_name.
        IF ls_tadir-obj_name <> ms_item-obj_name.
          rv_belongs_to_other_fugr = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD main_name.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = ms_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.
  METHOD serialize_functions.

    DATA:
      lt_source     TYPE TABLE OF rssource,
      lt_functab    TYPE ty_rs38l_incl_tt,
      lt_new_source TYPE rsfb_source,
      ls_function   LIKE LINE OF rt_functions.

    FIELD-SYMBOLS: <ls_func>          LIKE LINE OF lt_functab,
                   <ls_documentation> TYPE LINE OF ty_function-documentation.

    lt_functab = functions( ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      CLEAR ls_function.
      MOVE-CORRESPONDING <ls_func> TO ls_function.

      CLEAR lt_new_source.
      CLEAR lt_source.

      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = ls_function-global_flag
          remote_call             = ls_function-remote_call
          update_task             = ls_function-update_task
          short_text              = ls_function-short_text
          remote_basxml_supported = ls_function-remote_basxml
        TABLES
          import_parameter        = ls_function-import
          changing_parameter      = ls_function-changing
          export_parameter        = ls_function-export
          tables_parameter        = ls_function-tables
          exception_list          = ls_function-exception
          documentation           = ls_function-documentation
          source                  = lt_source
        CHANGING
          new_source              = lt_new_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc = 2.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error from RPY_FUNCTIONMODULE_READ_NEW' ).
      ENDIF.

      LOOP AT ls_function-documentation ASSIGNING <ls_documentation>.
        CLEAR <ls_documentation>-index.
      ENDLOOP.

      SELECT SINGLE exten3 INTO ls_function-exception_classes FROM enlfdir
        WHERE funcname = <ls_func>-funcname.              "#EC CI_SUBRC

      APPEND ls_function TO rt_functions.

      IF NOT lt_new_source IS INITIAL.
        strip_generation_comments( CHANGING ct_source = lt_new_source ).
        Lif_abapgit_object~mo_files->add_abap(
          iv_extra = <ls_func>-funcname
          it_abap  = lt_new_source ).
      ELSE.
        strip_generation_comments( CHANGING ct_source = lt_source ).
        Lif_abapgit_object~mo_files->add_abap(
          iv_extra = <ls_func>-funcname
          it_abap  = lt_source ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_function_docs.

    FIELD-SYMBOLS <ls_func> LIKE LINE OF it_functions.

    Lcl_abapgit_factory=>get_longtexts( )->serialize(
      iv_longtext_id = c_longtext_id_prog
      iv_object_name = iv_prog_name
      io_i18n_params = mo_i18n_params
      ii_xml         = ii_xml ).

    LOOP AT it_functions ASSIGNING <ls_func>.
      Lcl_abapgit_factory=>get_longtexts( )->serialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }|
        iv_longtext_id   = c_longtext_id_func
        iv_object_name   = <ls_func>-funcname
        io_i18n_params   = mo_i18n_params
        ii_xml           = ii_xml ).
      Lcl_abapgit_factory=>get_longtexts( )->serialize(
        iv_longtext_name = |LONGTEXTS_{ <ls_func>-funcname }___EXC|
        iv_longtext_id   = c_longtext_id_func_exc
        iv_object_name   = <ls_func>-funcname
        io_i18n_params   = mo_i18n_params
        ii_xml           = ii_xml ).
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_includes.

    DATA: lt_includes TYPE ty_sobj_name_tt.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lt_includes = includes( ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

* todo, filename is not correct, a include can be used in several programs
      serialize_program( is_item    = ms_item
                         io_files   = Lif_abapgit_object~mo_files
                         iv_program = <lv_include>
                         iv_extra   = <lv_include> ).

    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_texts.
    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = iv_prog_name
      AND language <> mv_language
      ORDER BY language ##TOO_MANY_ITAB_FIELDS.

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'LANGUAGE'
      CHANGING
        ct_tab = lt_tpool_i18n ).

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      READ TEXTPOOL iv_prog_name
        LANGUAGE <ls_tpool>-language
        INTO lt_tpool.
      <ls_tpool>-textpool = add_tpool( lt_tpool ).
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.
  ENDMETHOD.
  METHOD serialize_xml.

    DATA: lt_includes TYPE ty_sobj_name_tt,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = mv_language
      AND area = ms_item-obj_name.        "#EC CI_GENBUFF "#EC CI_SUBRC

    lt_includes = includes( ).

    ii_xml->add( iv_name = 'AREAT'
                 ig_data = lv_areat ).
    ii_xml->add( iv_name = 'INCLUDES'
                 ig_data = lt_includes ).

  ENDMETHOD.
  METHOD update_func_group_short_text.

    " We update the short text directly.
    " SE80 does the same in
    "   Program SAPLSEUF / LSEUFF07
    "   FORM GROUP_CHANGE

    UPDATE tlibt SET areat = iv_short_text
      WHERE spras = mv_language AND area = iv_group.

  ENDMETHOD.
  METHOD update_where_used.
* make extra sure the where-used list is updated after deletion
* Experienced some problems with the T00 include
* this method just tries to update everything

    DATA: lv_include LIKE LINE OF it_includes,
          lo_cross   TYPE REF TO cl_wb_crossreference.


    LOOP AT it_includes INTO lv_include.

      CREATE OBJECT lo_cross
        EXPORTING
          p_name    = lv_include
          p_include = lv_include.

      lo_cross->index_actualize( ).

    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    TYPES: BEGIN OF ty_stamps,
             user TYPE syuname,
             date TYPE d,
             time TYPE t,
           END OF ty_stamps.

    DATA:
      lt_stamps    TYPE STANDARD TABLE OF ty_stamps WITH DEFAULT KEY,
      lv_program   TYPE program,
      lv_found     TYPE abap_bool,
      lt_functions TYPE ty_rs38l_incl_tt.

    FIELD-SYMBOLS:
      <ls_function> LIKE LINE OF lt_functions,
      <lv_include>  LIKE LINE OF mt_includes_all,
      <ls_stamp>    LIKE LINE OF lt_stamps.

    lv_program = main_name( ).

    IF mt_includes_all IS INITIAL.
      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          program      = lv_program
        TABLES
          includetab   = mt_includes_all
        EXCEPTIONS
          not_existent = 1
          no_program   = 2
          OTHERS       = 3.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error from RS_GET_ALL_INCLUDES' ).
      ENDIF.
    ENDIF.

    " Check if changed_by for include object was requested
    LOOP AT mt_includes_all ASSIGNING <lv_include> WHERE table_line = to_upper( iv_extra ).
      lv_program = <lv_include>.
