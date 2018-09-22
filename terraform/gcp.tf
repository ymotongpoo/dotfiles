variable "instance_name" {
    type    = "string"
    default = "dev"
}

variable "go_version" {
    type    = "string"
    default = "1.11"
}

variable "gce_ssh_user" {
    type    = "string"
    default = "demo"
}
variable "gce_ssh_pub_key" {
    type    = "string"
    default = "/home/ymotongpoo/.ssh/terraform.pub"
}

variable "gce_ssh_private_key" {
    type    = "string"
    default = "/home/ymotongpoo/.ssh/terraform"
}

variable "docker_credential_gcr_ver" {
    type    = "string"
    default = "1.5.0"
}

locals {
    go_tarball = "go${var.go_version}.linux-amd64.tar.gz"
    skaffold_release_url = "https://github.com/GoogleContainerTools/skaffold/releases/download/v0.13.0/skaffold-linux-amd64"
    docker_credential_gcr_url = "https://github.com/GoogleCloudPlatform/docker-credential-gcr/releases/download/v${var.docker_credential_gcr_ver}/docker-credential-gcr_linux_amd64-${var.docker_credential_gcr_ver}.tar.gz"
}

/*
 * Set GOOGLE_CREDENTIALS enrionment variable that is the path to service
 * account JSON file.
 */
provider "google" {
    project     = "development-215403"
    region      = "asia-northeast1"
}

resource "google_compute_instance" "development" {
    name         = "${var.instance_name}"
    machine_type = "n1-standard-1"
    zone         = "asia-northeast1-a"
    description  = "development environment for testing"
    tags         = ["development"]

    boot_disk {
        initialize_params {
            size  = 30
            type  = "pd-standard"
            image = "debian-cloud/debian-9"
        }
    }

    network_interface {
        network = "default"

        access_config {
            # Ephemeral IP address
        }
    }

    service_account {
        scopes = ["userinfo-email", "compute-ro"]
    }

    metadata {
        "block-project-ssh-keys" = "true"
        "ssh-keys" = "${var.gce_ssh_user}:${file(var.gce_ssh_pub_key)}"
    }

    # install fundamental packages
    provisioner "remote-exec" {
        inline = [
            # add Cloud SDK repo
            "echo 'deb http://packages.cloud.google.com/apt cloud-sdk-$(lsb_release -c -s) main' | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list",
            "curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -",
            # preparation
            "sudo apt-get update && sudo apt-get upgrade -y",
            "sudo apt-get install -y build-essential git-core zsh python3 vim emacs",
            "sudo apt-get install -y google-cloud-sdk kubectl google-cloud-sdk-app-engine-go",
            # set zsh as default shell
            "sudo chsh -s /bin/zsh demo",
            # install Go
            "wget https://dl.google.com/go/${local.go_tarball}",
            "tar xf ${local.go_tarball}",
            "sudo mkdir -p /opt/go",
            "sudo chown demo /opt/go",
            "mv go /opt/go/go${var.go_version}",
            "rm ${local.go_tarball}",
        ]

        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }

    # install k8s
    provisioner "remote-exec" {
        inline = [
            "mkdir -p $HOME/bin",
            # install docker
            "sudo apt-get install -y apt-transport-https ca-certificates curl gnupg2 software-properties-common",
            "curl -fsSL https://download.docker.com/linux/debian/gpg | sudo apt-key add -",
            "sudo apt-key fingerprint 0EBFCD88",
            "sudo add-apt-repository \"deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -c -s) stable\"",
            "sudo apt-get update",
            "sudo apt-get install -y docker-ce",
            "sudo usermod -aG docker $USER",
            # install relevant tools
            "curl -fsSL ${local.docker_credential_gcr_url} | tar xz --to-stdout ./docker-credential-gcr > $HOME/bin/docker-credential-gcr && chmod +x $HOME/bin/docker-credential-gcr",
            # install k8s related tools
            "wget ${local.skaffold_release_url} -O $HOME/bin/skaffold",
            "chmod +x $HOME/bin/skaffold"
        ]
        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }

    # copy dot files
    provisioner "remote-exec" {
        inline = [
            "git clone https://github.com/ymotongpoo/dotfiles.git .dotfiles",
            "./.dotfiles/01-setup.sh"
        ]
        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }

    provisioner "file" {
        source      = "./setup.sh"
        destination = "setup.sh"

        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }

    timeouts {
        create = "10m"
        update = "10m"
        delete = "5m"
    }
}
