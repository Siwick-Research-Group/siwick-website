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
import webbrowser
from contextlib import suppress
from getpass import getpass
from os import listdir, walk
from os.path import getsize, isfile, join

try:
    from paramiko import SSHClient, AutoAddPolicy, AuthenticationException
    from tqdm import tqdm
except ImportError:
    print("paramiko and tqdm are required for this script to run.")
    sys.exit()

# The directory to mirror
CONTENT_DIR = "_rendered"
TARGET_DIR = "website"
CPM_USER = "laurenzk"

DESCRIPTION = """Update the Siwick Research Group website."""

EPILOG = """
Don't forget to render the website using the static site compiler `siwick-website`.
"""


parser = argparse.ArgumentParser(
    description=DESCRIPTION,
    epilog=EPILOG,
    formatter_class=argparse.RawTextHelpFormatter,
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

    password = getpass("CPM server password: ")

    with SSHClient() as client:

        client.set_missing_host_key_policy(AutoAddPolicy)
        try:
            client.connect(
                "gollum.physics.mcgill.ca", username=CPM_USER, password=password
            )
            print("Connected to CPM server.")
        except AuthenticationException as e:
            print(str(e))
            sys.exit()

        # Delete the current website content
        # This is to remove content that may be too old
        client.exec_command(f"rm -rf '{TARGET_DIR}/'")

        # Step 2: Calculate the transfer size
        with client.open_sftp() as sftp_client:
            total_bytes = sum(
                getsize(join(root, file))
                for root, _, files in walk(CONTENT_DIR)
                for file in files
            )

            with suppress(IOError):
                sftp_client.mkdir(TARGET_DIR)
            sftp_client.chdir(TARGET_DIR)

            # Step 3 : upload content
            upload_stream = put_dir(sftp_client, source=CONTENT_DIR, target="")

            with tqdm(
                desc="Upload to server", unit_scale=True, unit="B", total=total_bytes
            ) as pbar:
                for (bytes_transferred, fname) in upload_stream:
                    pbar.update(bytes_transferred)
                    pbar.write("\r {}".format(fname))

        # Step 4: sync with the domain
        out = client.exec_command(
            f"rsync -va --delete '{TARGET_DIR}/' /WWW/{CPM_USER}/siwicklab"
        )

    print("Upload done!")

    if arguments.show:
        print("Opening web page externally...")
        webbrowser.open("http://www.physics.mcgill.ca/siwicklab")
