# openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout target/key.pem -out target/cert.pem
# https://localhost:8000/examples/web/hello.html

HOST = "localhost"
PORT = 8000
import http.server
import ssl

Handler = http.server.SimpleHTTPRequestHandler
with http.server.HTTPServer((HOST, PORT), Handler) as httpd:
    print("Web Server listening at => " + HOST + ":" + str(PORT))
    sslcontext = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    sslcontext.load_cert_chain(keyfile="target/key.pem", certfile="target/cert.pem")
    httpd.socket = sslcontext.wrap_socket(httpd.socket, server_side=True)
    httpd.serve_forever()
