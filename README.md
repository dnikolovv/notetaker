# Notetaker: email-based notes ingestion

Notetaker processes inbound email and stores them as notes based on a set of
configured rules. It is designed to add notes to a vimwiki structure, to be
organised subsequently.

## Installation

TODO: add installation notes

## Usage

### Running notetaker

TODO: add run notes

### How it works

Notetaker can either run its own SMTP server on port 587 (encrypted with
TLS) or an HTTP server on port 6683 ("note") for a service like mailgun
to forward emails to.

When you send an email to notetaker, it runs a different mail processor
depending on the inbound address in order to create a note in your
personal notes repository.

In general, the mail processor takes the subject line as the title for
the note, in addition to the date, and places the content in a given
directory (it does not alter or process the content, simply copying it
directly from the body of the email). It then adds the notes an index
file which should be linked to elsewhere in your wiki. This index file
is generated from a template inside notetaker's configuration.

If there is a conflict (i.e. a note with that title already exists),
then notetaker will append a number to the end (incrementing with each
conflicted note with the same title).

If there are attachments in your email, notetaker will save them in a
folder for that note and link to them at the bottom of the note itself.

Notetaker will only accept inbound email from preconfigured addresses
and mail servers.

### Configuring notetaker

Notetaker is a fully-compiled programme, in that the configuration is
compiled as part of the programme itself. Since notetaker is written in
Haskell, the configuration files are also written in Haskell, and can
be found in `src/Data/`. You should write your own configuration files
in here, and then link them into `src/Data/App` in the `processors`
function.
