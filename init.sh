#!/bin/sh

PROJECT_DIR=~/work/ESA-Nowcasting-2024
git clone https://$GITHUB_TOKEN@github.com/InseeFrLab/ESA-Nowcasting-2023.git $PROJECT_DIR
chown -R onyxia:users $PROJECT_DIR/
cd $PROJECT_DIR

git config --global credential.helper store

mc cp -r s3/projet-esa-nowcasting/targets-data/ .
