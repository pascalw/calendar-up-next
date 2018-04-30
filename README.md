# Calendar-up-next

`calendar-up-next` is a small utility that prints upcoming (within 15 minutes) meetings in my Google
Calendar. I use it as a learning project for Haskell.

It's used in combination with [BetterTouchTool](https://updates.bettertouchtool.net/bettertouchtool_release_notes.html)
and looks like this:

![Calendar up next in Touch bar](touch-bar-screenshot.png)

To use it with your Google account follow these steps:

1. Create a Google service account in the [Google API console](https://console.developers.google.com/permissions/serviceaccounts).
2. Share the calendar you want to query with the email address of the service account.
3. Call the `calendar-up-next` script with the following parameters:

```sh
$ calendar-up-next \
  --calendar-account <your calendar email here> \
  --service-account <service account email here> \
  --service-account-key-path /path/to/auth.pem
```
