# Combine GPX

Create a single GPX-based activity out of multiple GPX files.

[On The Web!](https://combinegpx.coreyhaines.com/)


## Problem
GPX files are xml files that contain activity information generated by tracking applications, such as RunKeeper and Strava. Unfortunately, if I am tracking some errands, such as running the store, I often will stop one activity when I get there, then create a new one on the way home. This generates two activities, even though thye are conceptually a single one to me.

I want to have an easy way to combine these into a single activity.


## Solution

Since GPX files have a simple xml format, they can be manipulated fairly easily. This application allows you to select one or more of these files and combine them into a single one that can then be uploaded to your activity tracking system.

## Current Support

The initial version of the application only understands appending `trkpt` elements in the files.

```xml
<trkpt lat="42.007815000" lon="-87.677995000"><ele>183.5</ele><time>2020-08-17T19:23:51Z</time></trkpt>
```

## Implementation Status

- [x] Skeleton Layout
- [x] Add Files
- [ ] Remove File
- [ ] Reorder Files
- [ ] Download Combined File
