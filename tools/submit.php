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

function readSolutions($solutionsDir, $problemsDir) {
  $db = [];

  $files = glob($solutionsDir . '/*.txt');
  foreach($files as $file) {
    $problemID = -1;
    $match = [];
    if (!preg_match('/([0-9]+)\.txt/u', $file, $match)) {
      echo "Unable extract problem ID from filename\n";
      continue;
    }
    $problemID = (int)$match[1];

    $body = file_get_contents($file);
    if (empty($body)) {
      echo "Unable to read file '{$file}'\n";
      continue;
    }

    $problemFilename = $problemsDir . '/' . $problemID . '.txt';
    if (!file_exists($problemFilename) || !is_readable($problemFilename)) {
      echo "Unable to read problem file '{$problemFilename}'\n";
      continue;
    }

    $db[$problemID] = [
      'problem_id' => $problemID,
      'filename' => $file,
      'solution_spec_hash' => sha1($body),
      'problem_filename' => $problemFilename,
    ];
  }
  return $db;
}

function scoreSolution($solutionInfo) {
  $scoreTool = 'stack';
  if (!empty(getenv('STACK'))) {
    $scoreTool = getenv('STACK');
  }

  $scoreTool .= ' exec score-solution -- ';
  $score = @system($scoreTool . ' ' . $solutionInfo['problem_filename'] . ' ' . $solutionInfo['filename']);
  if (!is_numeric($score)) {
    $problemID = $solutionInfo['problem_id'];
    echo "$problemID: Unable to determine solution score. Defaulting to 0.0\n";
    return 0.0;
  }
  return (float)$score;
}

function submitSolution($solution) {
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

    curl_setopt($ch, CURLOPT_URL, 'http://2016sv.icfpcontest.org/api/solution/submit');
    curl_setopt($ch, CURLOPT_USERAGENT, 'Little Steel Bender ' . VERSION);

    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    curl_setopt($ch, CURLOPT_POST, 1);

    $realPath = realpath($solution['filename']);
    if (!file_exists($realPath) || !is_readable($realPath)) {
      throw new Exception("Solution file is not readable: '{$realPath}'!");
    }
    $curlFile = curl_file_create($realPath, 'text/plain', 'solution_spec');

    curl_setopt($ch, CURLOPT_POSTFIELDS, [
      'problem_id' => $solution['problem_id'],
      'solution_spec' => $curlFile,
    ]);


    // Debug
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
      $parsedResult = @json_decode($result, true);
      return [$parsedResult, $result];
    }
    else if ($status == 429) {
      usleep(1500000);
      continue;
    }
    else if (($status >= 400) && ($status <500)) {
      $parsedResult = @json_decode($result, true);
      return [$parsedResult, $result];
    }
    else if ($status > 500) {
      continue;
    }
    else {
      throw new Exception('HTTP error occured: ' . $status . ' (body:' . $result . ')');
    }
  }
  return [];
}



if (count($argv) < 3) {
  echo "Usage:\n";
  echo "{$argv[0]} <problems dir> <solutions dir>\n";
  exit(-1);
}

$problemsDir = $argv[1];
$solutionsDir = $argv[2];

echo "Reading submissions database...\n";
$db = readDB(SUBMISSION_DB);

echo "done!\n";
echo "DB contains " . count($db) .  " records\n";

echo "Reading solutions...\n";
$solutions = readSolutions($solutionsDir, $problemsDir);
echo "done! " . count($solutions) . " was found\n";

ksort($solutions);

$skipResubmit = true;
if (!empty(getenv('RESUBMIT'))) {
  $skipResubmit = !((bool)getenv('RESUBMIT'));
}

$dryRun = false;
if (!empty(getenv('DRYRUN'))) {
  $dryRun = (bool)getenv('DRYRUN');
}

$new = 0; $upToDate = 0; $improve = 0; $degrade = 0; $resubmit = 0;
$submitOk = 0; $submitFail = 0; $submitError = 0;
foreach($solutions as $problemID => $solution) {
  if (!isset($db[$problemID])) {
    echo "$problemID: New solution. Submitting...\n";
    $new++;
  }
  else {
    /* This solutions was on submission already */
    $dbRow = $db[$problemID];

    if ($dbRow['ok']) {
      /* Successfully submitted solution */
      if ($dbRow['solution_spec_hash'] == $solution['solution_spec_hash']) {
        echo "$problemID: Is up to date, skipping...\n";
        $upToDate++;
        continue;
      }

      $oldScore = $dbRow['resemblance'];
      $newScore = scoreSolution($solution);
      if ($newScore > $oldScore) {
        echo "$problemID: Improvement ({$oldScore} -> {$newScore}) detected, submitting...\n";
        $improve++;
      }
      else {
        echo "$problemID: Degrade ({$oldScore} -> {$newScore}) detected, skipping...\n";
        $degrade++;
        continue;
      }
    }
    else if ($dbRow['error'] == "Can not submit a solution to an own problem.") {
      echo "$problemID: Own problem, skipping...\n";
        continue;
    }
    else if ($dbRow['error'] == "Rate limit exceeded (per-hour limit).") {
      echo "$problemID: Solution was failed to submit last time due to rate limit. Resubmitting...\n";
      $resubmit++;
    }
    else {
      /* Solution previously was submitted with errors */
      echo "$problemID: Solution was failed to submit last time. Resubmitting...\n";
      $resubmit++;
      if ($skipResubmit) {
        continue;
      }
    }
  }

  if (!$dryRun) {
    list($info, $raw) = submitSolution($solution);

    if (!is_array($info) || !array_key_exists('ok', $info)) {
      echo "$problemID: Submit: bad response from server\n";
      $submitError++;
      continue;
    }
    if($info['ok']) {
      echo "$problemID: Submission accepted. Response: {$raw}\n";
      $submitOk++;
    }
    else {
      echo "$problemID: Submission rejected. Response: {$raw}\n";
      $submitFail++;

    }

    file_put_contents(SUBMISSION_DB . '/' . $problemID . '.json', $raw);
    $solutionBody = file_get_contents($solution['filename']);
    file_put_contents(SUBMISSION_DB . '/' . $problemID . '.solution.txt', $solutionBody);
    $db[$problemID] = $info;
  }
}

echo "Done processing\n";
echo "Solutions: {$new} new, {$resubmit} resubmit, {$improve} improved, {$degrade} degrades, {$upToDate} unchanged\n";
echo "Submit: {$submitOk} ok, {$submitFail} fail, {$submitError} error\n";
