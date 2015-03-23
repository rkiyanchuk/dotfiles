// vimperator-clearly.js 0.0.2
// Copyright (c) 2014 Moza USANE
// This software is released under the MIT License.
// http://opensource.org/licenses/mit-license.php
// GitHub: https://github.com/mozamimy/vimperator-clearly.js

let INFO = xml`
  <plugin
    name="vimperator-clearly.js"
    version="0.0.2"
    href="http://github.com/mozamimy/vimperator-clearly.js"
    summary="A simple vimperator plugin for Evernote Clearly."
    xmlns="http://vimperator.org/namespaces/liberator"
  >
  <author email="mozamimy@quellencode.org">Moza USANE</author>
  <license>MIT License</license>
  <project name="Vimperator" minVersion="3.0"/>
  <item>
  <tags>:clearly</tags>
  <spec>:clearly</spec>
  <description>
    <p>A simple vimperator plugin for Evernote Clearly.</p>
  </description>
  </item>
  </plugin>
`;

(function() {
  window.onload = function() {
    require("chrome://readable-by-evernote/content/global.js");
  }

  commands.addUserCommand(
    ["clearly"],
    "Toggle Evernote Clearly page.",
    function(args) {
      __readable_by_evernote.context_menu__call();
    }
  );
})();
