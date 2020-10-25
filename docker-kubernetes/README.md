# Introduction to Docker and Kubernetes

Since the start of Docker in 2013, containers have become widely adopted by most tech companies as a way to efficiently develop and deploy software in production. For instance, Google, as the original author of Kubernetes, deploys all of its services in containers.

In this talk, you will be introduced the basic concepts of containers, and how to deploy a simple REST API written in Go inside of a Docker container. You will then be introduced to Kubernetes, an open-source container orchestration system, which can automatically manage and deploy containers. We will be deploying our REST API application inside a Kubernetes cluster while exposing its endpoints to the outside world.