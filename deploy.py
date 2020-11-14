# -*- coding: utf-8 -*-
""" 
Script that updates the Siwick Research Group website. Modify the files for the
website in the 'website' folder first. See script help:

    >>> python deploy.py --help

You will need to know the Siwick research group's CPM server password.

This script requires:
    - Python 3.3+
    - paramiko
    - tqdm
"""
import argparse
import sys
from contextlib import suppress
from getpass import getpass
from os import listdir, walk, environ
from os.path import getsize, isfile, join

try:
    from paramiko import SSHClient, AutoAddPolicy, AuthenticationException
    from tqdm import tqdm
except ImportError:
    print("paramiko and tqdm are required for this script to run.")
    sys.exit(-1)

# The directory to mirror
CONTENT_DIR = "_rendered"
TARGET_DIR = "website"

DESCRIPTION = """Update the Siwick Research Group website. By default, pdfs are not uploaded unless the '--all' flag is toggled."""

EPILOG = """
Don't forget to render the website using the static site compiler `siwick-website`.
"""


parser = argparse.ArgumentParser(
    description=DESCRIPTION,
    epilog=EPILOG,
    formatter_class=argparse.RawTextHelpFormatter,
)
parser.add_argument(
    "--all",
    action="store_true",
    help="Upload all files (including large files like pdfs)",
)
parser.add_argument(
    "--dont-ask-password",
    action="store_true",
    help="Don't ask for a password. If the password is not found in the environment variable DEPLOY_PASSWORD, an error is raised."
)
parser.add_argument(
    "--show",
    action="store_true",
    help="Navigate to website with default web browser after deployment",
)


def put_dir(client, source, target, exclude_ext=tuple()):
    """ 
    Upload the contents of the source directory to the target path, including subdirectories. 
    
    Parameters
    ----------
    client : paramiko.SFTPClient
        
    source, target : str or path-like
        Source directory and target directory, respectively.
    
    Yields
    ------
    size : int
        Bytes transferred.
    message : str
        Message specifying the filename, and whether it was transferred or skipped.
    """
    # string.endswith must be compared with tuples of strings
    exclude_ext = tuple(exclude_ext)

    for item in listdir(source):
        src_path = join(source, item)
        dst_path = item

        if isfile(src_path):
            if src_path.endswith(exclude_ext):
                yield (getsize(src_path), "skipped:     " + str(src_path))
            else:
                yield (
                    client.put(src_path, dst_path).st_size,
                    "transferred: " + str(src_path),
                )

        else:
            with suppress(IOError):
                client.mkdir(dst_path)
            client.chdir(dst_path)
            yield from put_dir(client, src_path, dst_path, exclude_ext=exclude_ext)
            client.chdir("..")


if __name__ == "__main__":

    arguments = parser.parse_args()

    # On Github, the password is stored in environment variables
    try:
        password = environ["DEPLOY_PASSWORD"]
        print('Password located in environment variables.')
    except KeyError:
        # The following flag prevents this script from hanging in
        # a cloud environment, e.h. Github Actions.
        if arguments.dont_ask_password:
            print('Password not found; aborting.')
            sys.exit(-1)
        else:
            password = getpass("CPM server password: ")

    if arguments.all:
        exclude_ext = tuple()
    else:
        exclude_ext = (".pdf",)

    with SSHClient() as client:

        client.set_missing_host_key_policy(AutoAddPolicy)
        try:
            client.connect(
                "gollum.physics.mcgill.ca", username="decotret", password=password
            )
            print("Connected to CPM server.")
        except AuthenticationException as e:
            print(str(e))
            sys.exit(-1)

        # Step 0: Calculate the transfer size
        with client.open_sftp() as sftp_client:
            total_bytes = sum(
                getsize(join(root, file))
                for root, _, files in walk(CONTENT_DIR)
                for file in files
            )

            with suppress(IOError):
                sftp_client.mkdir(TARGET_DIR)
            sftp_client.chdir(TARGET_DIR)

            # Step 1 : upload content
            upload_stream = put_dir(
                sftp_client, source=CONTENT_DIR, target="", exclude_ext=exclude_ext
            )

            with tqdm(
                desc="Upload to server", unit_scale=True, unit="B", total=total_bytes
            ) as pbar:
                for (bytes_transferred, fname) in upload_stream:
                    pbar.update(bytes_transferred)
                    pbar.write("\r {}".format(fname))

        # Step 2: sync with the domain
        out = client.exec_command(f"rsync -a '{TARGET_DIR}/' /WWW/decotret/siwicklab")

    print("Upload done!")
    sys.exit()
