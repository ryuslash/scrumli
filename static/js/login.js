function login ()
{
    navigator.id.get(function(assertion) {
        if (assertion) {
            var assertion_field =
                    document.getElementById("assertion-field");
            assertion_field.value = assertion;
            var login_form = document.getElementById("login-form");
            login_form.submit();
        }
    });
}
