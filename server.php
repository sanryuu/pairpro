<?php

if ( empty($_POST) ) {  // GET 

     echo file_get_contents("/tmp/" . $_GET["name"]);


} else {  // POST

     file_put_contents("/tmp/" . $_POST["name"] ,$_POST["body"] );

}
