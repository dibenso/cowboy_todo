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
    On osx this can be done by using homebrew like: $ brew install postgres
    2. Ensure you have a $PG_USER environment variable. This will be used as the username for connecting to postgres.
    3. Ensure you have a $TODO_DB environment variable. This will be used as the database for connecting to postgres.
    4. Ensure you have a $PG_PASS environment variable. This will be used as the user password for connecting to postgres.

Build
-----

    $ make

Run
---
    $ make run
    
Routes
------
PUT /users/registration --- Registers a user   
Example:
    
`$ curl -i -X PUT -H "Content-Type: application/json" --data '{"email": "email@example.com", "username": "example_user", "password": "example_password"}' 127.0.0.1:8080/users/register`    

POST /users/login --- login as a user    
Example:    

`$ curl -i -X POST -H "Content-Type: application/json" --data '{"email": "email@example.com", "password": "example_password"}' 127.0.0.1:8080/users/login`    
The response will give you a session token back for use with authorized requests.   
This response should look something like:    
`{"status":"ok","data":{"username":"example_user","token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo2Mn0.f_oKJRnV11nFkaseYjdPQ48Ac1w6a2G7FZUAuAjXHnk"}}`    

POST /todo/create --- create a todo for the currently logged in user    
Example:    

`curl -i -X POST -H "Content-Type: application/json" -H "Authorization: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo2Mn0.f_oKJRnV11nFkaseYjdPQ48Ac1w6a2G7FZUAuAjXHnk" --data '{"todo_title": "New Todo", "todo_body": "Description of todo"}' 127.0.0.1:8080/todo/create`    
Notice how we used the token from the login response as the Authorization header to create a new todo.    

       
GET /todos --- Get's all todos for the currently logged in user    
Example:    
    
`curl -i -X GET -H "Authorization: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo2Mn0.f_oKJRnV11nFkaseYjdPQ48Ac1w6a2G7FZUAuAjXHnk" 127.0.0.1:8080/todos`    

    
GET /todo/:todo_id --- Get a single todo for the currently logged in user    
Example:    
    
`curl -i -X GET -H "Authorization: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo2Mn0.f_oKJRnV11nFkaseYjdPQ48Ac1w6a2G7FZUAuAjXHnk" 127.0.0.1:8080/todo/7`    

    
POST /todo/update/:todo_id --- Updates a todo for the currently logged in user   
Example:    
    
`curl -i -X POST -H "Content-Type: application/json" -H "Authorization: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo2Mn0.f_oKJRnV11nFkaseYjdPQ48Ac1w6a2G7FZUAuAjXHnk" --data '{"field": "title", "value": "New Todo!"}' 127.0.0.1:8080/todo/update/7`    
Note in the JSON body "field" indicates which column of the todos table you want to update. This can be "title" or "body" and "value" indicates the new value for that column.    
    
DELETE /todo/delete/:todo_id --- Deletes a todo for the currently logged in user   
Example:   
    
`curl -i -X DELETE -H "Authorization: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo2Mn0.f_oKJRnV11nFkaseYjdPQ48Ac1w6a2G7FZUAuAjXHnk" 127.0.0.1:8080/todo/delete/7`    
     