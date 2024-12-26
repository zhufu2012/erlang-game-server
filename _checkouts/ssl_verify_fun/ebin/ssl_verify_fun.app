{application,ssl_verify_fun,
             [{description,"SSL verification library"},
              {vsn,"1.1.6"},
              {modules,[ssl_verify_fingerprint,ssl_verify_fun_cert_helpers,
                        ssl_verify_fun_encodings,ssl_verify_hostname,
                        ssl_verify_pk,ssl_verify_string,ssl_verify_util]},
              {registered,[]},
              {applications,[kernel,stdlib,ssl]},
              {env,[]}]}.
