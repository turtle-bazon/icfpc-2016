<?php /* -*- mode: php; mode: auto-fill; indent-tabs-mode: nil; c-basic-offset: 2; -*- */

require_once('common.inc.php');

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

$new = 0;
foreach($problems as $problem) {
  $problemID = $problem['problem_id']; $problemHash = $problem['problem_spec_hash'];
  $problemFile = 'problems/' . $problemID . '.txt';
  if (file_exists($problemFile)) {
    $problemBody = file_get_contents($problemFile);
    $problemFileHash = sha1($problemBody);
    if ($problemFileHash == $problemHash) {
      echo "Problem {$problemID} is already present. Skipping...\n";
      continue;
    }
  }

  echo "Fetching problem " . $problemID . " (" . $problemHash . ")\n";
  $resp = httpGet('http://2016sv.icfpcontest.org/api/blob/' . $problemHash, false);
  file_put_contents($problemFile, $resp);
  $new++;
}

echo "Snapshot : $snapshotHash (" . date("r", $snapshotTime) . ") is done\n";
echo "Problems: " . count($problems) . " total, " . $new . " new\n";
