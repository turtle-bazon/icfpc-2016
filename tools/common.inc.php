<?php /* -*- mode: php; mode: auto-fill; indent-tabs-mode: nil; c-basic-offset: 2; -*- */

define('VERSION', 'v0.0.1');

$ch = curl_init();
$lastQuery = 0;

$stackTool = 'stack';
if (!empty(getenv('STACK'))) {
  $stackTool = getenv('STACK');
}

function httpGet($url, $parseJSON = true) {
  global $ch,$lastQuery;

  for(;;) {
    curl_setopt($ch, CURLOPT_HTTPHEADER, [
      'Expect:',
      'X-API-Key: 41-736c4fa841ea2c5e62c5991086b87886'
    ]);
    /* Ask for any compression that server supports */
    curl_setopt($ch, CURLOPT_ENCODING, "");

    /* curl '-L' option simulation */
    curl_setopt($ch, CURLOPT_FOLLOWLOCATION, true);
    curl_setopt($ch, CURLOPT_AUTOREFERER, true);

    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_USERAGENT, 'Little Steel Bender ' . VERSION);

    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    //Debug
    // curl_setopt($ch, CURLOPT_VERBOSE, true);

    /* Delay */
    $now = microtime(true);
    $delay = $lastQuery - $now + 1.5;
    if ($delay > 0) {
      usleep((int)$delay*1000000);
    }

    $result = curl_exec($ch);
    $lastQuery = $now;

    $status = curl_getinfo($ch, CURLINFO_HTTP_CODE);

    if ($status == 200) {
      if ($parseJSON) {
        return json_decode($result, true);
      }
      else {
        return $result;
      }
    }
    else if ($status == 429) {
      usleep(1500000);
      continue;
    }
    else if ($status > 500) {
      continue;
    }
    else {
      throw new Exception('HTTP error occured: ' . $status . ' (body:' . $result . ')');
    }
  }
}
