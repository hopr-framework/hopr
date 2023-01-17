## Docker
Docker allows to run the application in a [docker container](https://www.docker.com/resources/what-container/), encapsulated from your host machine.
The container is launched from an [image](https://docs.docker.com/get-started/overview/#docker-objects) which you can download and run immediately, irrespective of your architecture and OS.


### Install docker
To be able to run a docker container, you must  [Install Docker Desktop](https://docs.docker.com/desktop/) and start it. 

On Mac, it is recommended to read the [Mac OS permission requirements](https://docs.docker.com/desktop/mac/permission-requirements/)

On Windows, it is recommended to read the [Windows permission requirements](https://docs.docker.com/desktop/windows/permission-requirements/>)

On Linux, if you do not want to preface the docker command with `sudo`, you can create a [Unix group](https://docs.docker.com/engine/install/linux-postinstall/) called `docker` and add your user to it. If you are uncomfortable with running sudo, you can try to [run docker in "rootless" mode](https://docs.docker.com/engine/security/rootless/>).

### Some useful docker commands
- startup docker locally (needs root priviledges...)
    ```
    systemctl start docker
    ```
- pull an image from the github registry
    ```
    docker pull ghcr.io/xxx/xxx:xxx
    ```
- list all local images
    ```
    docker images
    ```
- remove image locally
    ```
    docker image rm <imageID>
    ```
- start an image interactively, this **creates a new container**
    ```
    docker run -it <imageID>
    ```
  The option ``-i`` stands for interactive while ``-t`` gives you a terminal.

- start an image an mirror a local folder into the container (path on container is created)
    ```
    docker run -it -v path_on_host:path_on_container <imageID> 
    ```
- list all containers
    ```
    docker ps -a
    ```
- remove a container
    ```
    docker container rm <contID>
    ```
- start a container and then start the interactive session
    ```
    docker start <contID>
    docker attach <contID>
    ```
- locally build new image from file called "Dockerfile" that must lie in the current directory (which must also be passed by `.`)
    ```
    docker build -t imagerepo:tag -f Dockerfile . 
    ```
- locally make a new image from a container
    ```
    docker commit <contID> repository:tag
    ```
