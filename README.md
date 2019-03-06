# MBTA board

## Goal

To get something as close to
https://commons.wikimedia.org/wiki/File:North_Station_departure_board.JPG as
possible.

## Setup

Get [Elm](https://elm-lang.org/)

## Usage

Run `elm-reactor`.

You can also go to http://kenhkan-mbta.s3-website-us-east-1.amazonaws.com/ for
a live version.

## Notes

1. Currently only North Station and South Station are supported.
2. It lists the next 15 trains departing from the select station starting now.
3. If a train is leaving in the same minute as the current time, it would be
   listed as "Departing" under status.
4. It is sorted by departure time.
5. If we are close to the end of day, it lists the next trains departing the
   next morning.
6. The board refreshes itself every 30 seconds.
