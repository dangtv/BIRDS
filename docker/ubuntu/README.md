### Docker

* Build a docker image:
  ```bash 
    docker build -t "birds" .
  ```

* Run a docker container based on this image
  ```bash 
    docker run --name "birds1" -ti -p 5432:5432 -p 5050:5050 -v <host_folder>:<container_folder> birds
  ```
  
 * For example:
    ```bash 
      docker run --name "birds1" -ti -p 5432:5432 -p 5050:5050 -v $(pwd)/examples:/root/examples birds
    ```