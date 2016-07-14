Todo App
=====

A Todo REST app written in Erlang using cowboy with cowboy_rest.

Requests
--------

PUT http://localhost:8080/users/register   
`curl -X PUT -d '{"username": "username123", "password": "abc123abc123", "email": "email@example.com"}' -H "Accept:application/json" -H "Content-Type:application/json" http://localhost:8080/users/register/`


Setup
-----
    1. Install postgres.
    2. Ensure you have a $PG_USER environment variable. This will be used as the username for connecting to postgres.
    3. Ensure you have a $TODO_DB environment variable. This will be used as the database for connecting to postgres.
    4. Ensure you have a $PG_PASS environment variable. This will be used as the user password for connecting to postgres.

Build
-----

    $ make

Run
---
    $ make run
