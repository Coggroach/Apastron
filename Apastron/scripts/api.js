var getGraphByUsername = function(username, onSuccess, onError)
{
  $.ajax(
    { url: '/graph/' + encodeURIComponent(username) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}