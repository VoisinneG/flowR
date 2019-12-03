
function toggle(anId)
{
  var content = document.getElementById(anId);
  
  if (content.style.maxHeight){
      content.style.maxHeight = null;
  } else {
      content.style.maxHeight = content.scrollHeight + "px";
  }
    
}

window.onload = function () { 
  //Hide("foo");	
};
