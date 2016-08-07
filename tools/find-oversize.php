<?php /* -*- mode: php; mode: auto-fill; indent-tabs-mode: nil; c-basic-offset: 2; -*- */

require_once('common.inc.php');

define('SUBMISSION_DB', 'submits');

function readDB($dbPath) {
  $db = [];

  $files = glob($dbPath . '/*.json');
  foreach($files as $file) {
    $problemID = -1;
    $match = [];
    if (!preg_match('/([0-9]+)\.json/u', $file, $match)) {
      echo "Unable extract problem ID from filename\n";
      continue;
    }
    $problemID = (int)$match[1];
    $dbRow = file_get_contents($file);
    if (empty($dbRow)) {
      echo "Unable to read file '{$file}'\n";
      continue;
    }
    $dbRow = json_decode($dbRow, true);
    if (!$dbRow) {
      echo "Unable to decode file '{$file}'\n";
      continue;
    }

    $db[$problemID] = $dbRow;
  }
  return $db;
}

echo "Reading submissions database...\n";
$db = readDB(SUBMISSION_DB);

echo "done!\n";
echo "DB contains " . count($db) .  " records\n";

foreach ($db as $problemID => $row) {
  if (!$row['ok'] && ($row['error'] == 'Invalid solution spec: Solution size limit exceeded.')) {
    echo "problems/{$problemID}.txt\n";
  }
}
