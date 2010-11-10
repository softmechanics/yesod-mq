{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , FlexibleContexts
           #-}

module Yesod.Mq.Widget where

import Yesod
import Yesod.Mq.Core

-- | Initialize message queue
initMq :: (YesodSubRoute Mq master) => GWidget sub master ()
initMq = do
  addScript $ subRoute MqJsR
  addJulius [$julius|
     $(document).ready(function () { 
     $MQ("onLoad", {}); 
   });
  |]

-- | Subscribe to queues matching pattern.  Message available in javascript object "msg"
mql :: String -> Julius (Route master) -> GWidget sub master ()
mql q hndl = addJulius [$julius|
    $MQL(%q'%, function (msg) {
      ^hndl^
    });
  |]
  where q' = if regex q then q else '"' : q ++ ['"']
        regex s = head s == '/' && last s == '/'

-- | Publish message on queue
mq :: String -> Julius (Route master) -> GWidget sub master ()
mq q msg = addJulius [$julius| $MQ("%q%", ^msg^); |]

-- | Add code to run on $(document).ready()
onLoad :: Julius (Route master) -> GWidget sub master ()
onLoad = mql "onLoad"

-- | A log window widget, logs Mq messages
yuiLogger :: GWidget sub master ()
yuiLogger = do
  --CSS file (default YUI Sam Skin) --
  addStylesheetRemote "http://yui.yahooapis.com/2.8.2/build/logger/assets/skins/sam/logger.css"

  -- Dependencies --
  addScriptRemote "http://yui.yahooapis.com/2.8.2/build/yahoo-dom-event/yahoo-dom-event.js"
 
  -- OPTIONAL: Drag and Drop (not required if not enabling drag and drop) --
  addScriptRemote "http://yui.yahooapis.com/2.8.2/build/dragdrop/dragdrop-min.js"
 
  -- Source file --
  addScriptRemote "http://yui.yahooapis.com/2.8.2/build/logger/logger-min.js"

  onLoad [$julius|
    var container = document.body.appendChild(document.createElement("div"));
    container.setAttribute("class", "yui-skin-sam");
    var logDiv = container.appendChild(document.createElement("div"));
    var logReader = new YAHOO.widget.LogReader(logDiv);
  |]

  mql "log" [$julius| YAHOO.log(msg.payload.msg, msg.payload.level); |]

-- | configure log level for queues matching pattern
yuiLog :: String -> String -> GWidget sub master ()
yuiLog pattern level = 
    mql pattern [$julius|
if (msg.name != "log") {
  // prevent feedback loop
  $MQ("log", {msg:msg.toSource(), level:"%level%"});
}
|]
