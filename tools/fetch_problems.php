<?php /* -*- mode: php; mode: auto-fill; indent-tabs-mode: nil; c-basic-offset: 2; -*- */

define('VERSION', 'v0.0.1');

$ch = curl_init();
$lastQuery = 0;

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

$resp = httpGet('http://2016sv.icfpcontest.org/api/snapshot/list');

if (!$resp['ok']) {
  echo "Error while fetching snapshot list: '" . $resp['error'] . "\n";
  die(1);
}

$snapshotList = $resp['snapshots'];

if (empty($snapshotList)) {
  echo 'No snapshots found...'; die(1);
}

/* Sort by 'snapshot_time' DESC */
usort($snapshotList, function ($l, $r) {
  return ($r['snapshot_time'] - $l['snapshot_time']);
});

/* Take the lastest */
$snapshotTime = $snapshotList[0]['snapshot_time'];
$snapshotHash = $snapshotList[0]['snapshot_hash'];

echo "Processing snapshot: $snapshotHash (" . date("r", $snapshotTime) . ")\n";

$resp = httpGet('http://2016sv.icfpcontest.org/api/blob/' . $snapshotHash);

$problems = $resp['problems'];

echo "Found " . count($problems) . " problems\n";

foreach($problems as $problem) {
  $problemID = $problem['problem_id']; $problemHash = $problem['problem_spec_hash'];
  echo "Fetching problem " . $problemID . " (" . $problemHash . ")\n";
  $resp = httpGet('http://2016sv.icfpcontest.org/api/blob/' . $problemHash, false);
  file_put_contents('problems/' . $problemID . '.txt', $resp);
}

echo "Done!\n";
