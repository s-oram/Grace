<?php


class LucidityMasterKeyGen {
    private $Salts = array();
    private $SaltCount = 25;

    function __construct(){
        $this->Salts[] = 'fQK8g4t97GLTWwp_IPjymy6MVKpd3H5G';
        $this->Salts[] = 'f9SnzEcqi4sYuE9GLTWwpIUp5Z45kP5S6LjDQu';
        $this->Salts[] = 'yGOThXI6D5L0uE9duATLydZBF3VnLLX1T0H';
        $this->Salts[] = 'FW1kxT3oTGqdjcXRNzBWO4OATLydZCFPbNkwjD';
        $this->Salts[] = 'wXG7rNtGcWNC1OZUtuV_jEnkrrHWqGXD';
        $this->Salts[] = 'ZaKV_jJizx0OZEVbZHw56lurHWqsvr58KhYA';
        $this->Salts[] = 'l6gpzsZI04EUvaxFpMVosSs_R8nctHz9';
        $this->Salts[] = '5lk88c9k3DK9GnTTcOgLBr79t8nk9HebAd';
        $this->Salts[] = 'vQ4BRPiQrrtc2LKnyYh7UrQji5bMoCA_';
        $this->Salts[] = '8ZfAxhGFPFsF3U3yYh7UrQji5bzw9Hd6C0Cn0SkOOd1';
        $this->Salts[] = 'AF1qmVA9S5Wh0SYc49h69huT_IRXURSW';
        $this->Salts[] = 'cYVvu6lJvT_Iq3tuNmy9Z3oxT2T_IG6eq3WgeL';
        $this->Salts[] = 'IYZkTza3xsXvSbyzBNI74eP2fsQwZOCM';
        $this->Salts[] = 'S2yDItKivoFBuS_sszCAIK0zBNI7Fvc837oxE';
        $this->Salts[] = '0PYG110ij9JES1Q7PsPkT0duGFHACzSf';
        $this->Salts[] = 'QbyHpxLXBXnlbSl1hxPkT0d1HV62Jfbmw7na6';
        $this->Salts[] = 'kt1OD7k2AxTFcnrqAO_v4fOt5mKZvxtP';
        $this->Salts[] = '5WDIxdIugdB6z4epr4juOOzWvGSnt5mKZe1_n';
        $this->Salts[] = 'hSQdZRrcXrJ2ZL5EJd1ZF3Q3UEkUw7v5';
        $this->Salts[] = 'THySTeddq2SoZL5EJd1ZF3Q3UU8oulNG1jCMOqQIrvHBt';
        $this->Salts[] = 'sIu4nt2dq54zh0mjJxlGMRMBZL5EJd1ZF3Q3U7Fqed6n';
        $this->Salts[] = 'fqBNNVa7DernVuhlUwdDlWRTVnja77ejrva0';
        $this->Salts[] = 'MNvJRJOjMuhlU8R_A0pJ_9k6c3d59Acl';
        $this->Salts[] = 'ZOZX4DJV8JmnnObzSuhlUuhl6c3hlUoIicH9LfZwC';
        $this->Salts[] = 'Wd0U6c3dLpZUVUvE8NupniaEzuH6c3J1IFpuLm';
    }

    private function GetRawKeyCheckString($KeyCheckIndex, $UserName, $UserEmail){
        $sx = $this->Salts[$KeyCheckIndex];
        return $sx . $UserName . $sx . $UserEmail . $sx;
    }

    function GenerateKey($UserName, $UserEmail) {

        $ChecksumData = $UserName . $UserEmail;

        $Data = array();
        $Data[] = 'License Key File for Lucidity 1.0';
        $Data[] = 'BEGINKEY>>>>';
        $Data[] = "$UserName";
        $Data[] = "$UserEmail";

        for ($c1 = 0; $c1 < $this->SaltCount; $c1++) {
            $sx = $this->Salts[$c1];
            $KeySection = $this->GetRawKeyCheckString($c1, $UserName, $UserEmail);
            $Data[] = strtoupper(md5($KeySection));
            $ChecksumData = $ChecksumData . strtoupper(md5($KeySection));
        }
        $Data[] = strtoupper(md5($ChecksumData));
        $Data[] = '<<<<ENDKEY';

        return $Data;
    }
}



//===== Generate the key data =====================
$KeyGen = new LucidityMasterKeyGen();
$KeyData = $KeyGen->GenerateKey('James Brown', 'jb@hotmail.com');
//==================================================



//=====  Display the key data in the browser =======
foreach ($KeyData as $Text) {
    echo htmlentities($Text) . '<br>';
}
//==================================================



//===== Creates a text file from the key data =====
$UserName = 'James Brown';
$SanitisedName = htmlentities($UserName, ENT_COMPAT, "UTF-8");
$FileName = "bin/Lucidity Key For $SanitisedName.dat";
$FileText = '';

foreach ($KeyData as $Text) {
    $FileText = $FileText . $Text . "\n";
}
file_put_contents($FileName, $FileText);
//==================================================







//====== end of file ======