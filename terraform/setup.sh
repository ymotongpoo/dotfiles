#!/bin/bash
## init
gcloud init

## docker with GCR
gcloud auth configure-docker

## kubectl with GKE
gcloud config set project yoshifumi-cloud-demo
gcloud config set compute/zone us-central1-a
# gcloud container clusters create midroservices-demo
gcloud container clusters get-credentials microservices-demo
