variable "go_version" {
    type    = "string"
    default = "1.11"
}

variable "go_tarball" {
    type    = "string"
    default = "go1.11.linux-amd64.tar.gz"
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

/*
 * Set GOOGLE_CREDENTIALS enrionment variable that is the path to service
 * account JSON file.
 */
provider "google" {
    project     = "development-215403"
    region      = "asia-northeast1"
}

resource "google_compute_instance" "development" {
    name         = "development"
    machine_type = "n1-standard-1"
    zone         = "asia-northeast1-a"
    description  = "development environment for testing"
    tags         = ["development"]

    boot_disk {
        initialize_params {
            size  = 10
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
            # preparation
            "sudo apt-get update && sudo apt-get upgrade -y",
            "sudo apt-get install -y build-essential zsh python3 vim emacs",
            # set zsh as default shell
            "sudo chsh -s /bin/zsh demo",
            # install Go
            "wget https://dl.google.com/go/${var.go_tarball}",
            "tar xf ${var.go_tarball}",
            "sudo mkdir -p /opt/go",
            "sudo chown demo /opt/go",
            "mv go /opt/go/go${var.go_version}",
            "rm ${var.go_tarball}"        ]

        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }

    # copy .zshrc and .zshenv
    provisioner "file" {
        source      = "../.zshrc"
        destination = ".zshrc"

        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }

    provisioner "file" {
        source      = "../.zshenv.linux"
        destination = ".zshenv"

        connection {
            type        = "ssh"
            user        = "demo"
            agent          = true
            agent_identity = "${file(var.gce_ssh_private_key)}"
        }
    }
}
