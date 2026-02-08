---
layout: post
title:  "Git dev branch attempts and failures"
date:   2025-02-17 12:39
modified_date: 2026-02-08 06:07
categories: github
---
Almost everyone starts out using git by committing directly to the mainline branch. As they mature, and begin to use a project that has users, or, god forbid, other developers, this simple and efficient workflow gives way to a feature-branches-based one. It's safe, it's common, it's encouraged by the PR-based workflow nigh every project nowadays relies on. I have used it for a very long time, despite something about it feeling wrong to me, like it has added a burden, made committing less fun, added an extra cost to every fix and every feature. It feels like drudgery that I force myself to do to be proper.

I despair about all the dead time. Think about the cumulative time that gets lost creating and deleting branches. "It's one command each, Jan." I know, but it's still dead time.

And I dislike deleting branches. I always hesitate before I press the button. Thus, despite being one command, it does incur a cost in my mind.

Despite all this, I don't think committing straight to main is better. Everything has its tradeoffs. It's better in some ways (simple, fast) and worse in others (no protection for main, extra hesitance to commit), but on the whole I'd say it's worse. If my only choices were between straight-to-main workflow (trunk-based?) and feature-branches workflow, I'd pick the latter. Maybe there can be a workflow that is somewhere in between?

My requirements:

1. Protection for main. Nothing that involves messing with it directly.
2. No extra manual work every time. I want my life to be simple. I don't want to waste time checking out different branches, and on ensuring I'm on the correct branch. Don't add burden on the developer.
3. Encouraging 'commit early and often'. Ugly, tiny commits? No problem. No one's going to see it on main.

{% include attention.html content="I don't come to any satisfying conclusion at the end. I have still not found a workflow I like, and this article only chronicles my attempts." %}

## Failed attempt 1: Dev branch + squash merge
My first idea was to have one other branch in addition to main. I called it dev. Commit straight to it as you would main. Commit early and often, don't worry about commit messages. Then when it comes the time to integrate the changes, make a PR, and use squash merge.

This only works up to the point after merging your first PR, when you make some more changes and try to make another. In this second PR, all the commits from the first PR are also present. As you keep going and make more and more commits, this will accumulate infinitely.

There are also cases where it'd be better to have several commits instead of only one, but it will always be only one with squash merge. I find it hard to give up squash merge because it makes it easy to meet requirement 3; can commit as much as you like without worrying, knowing it will get squashed in the end, without having to do the squash manually yourself.

## Failed attempt 2: Dev branch + rebase + squash merge
Ostensibly, you can work around the issue from attempt 1 by first rebasing. First ensure main is up to date:

```sh
git fetch origin main:main
```

Then rebase dev on main:

```sh
git rebase origin/main dev
```

(the `dev` is not needed in the command if you are already in dev)

Or equivalently, `git pull --rebase origin main`.

After you do this, you're able to make a PR with just the new commits, but you lose your history on dev. I mean if you had a lot of small commits and squash-merged the PR, it will become in the history just the new squash commit instead of the small commits. They are still there on the PR I guess, so you may or may not care.

alias:

```sh
git config --global alias.prepr '!git fetch origin main:main && git switch dev && git rebase origin/main --autostash && git push --force origin dev'
```

so it can simply be run with `git prepr`. Each time you want to make a PR, run this, then create a PR from dev on Github.

I still thought if there is a way to also keep the entire history of it and not have to rebase it each time I would prefer that.

(But wanting to preserve history on the 2nd branch is adding another requirement that I didn't mention at the outset, so is it really fair to called this a 'failed attempt'? I guess it's an 'unsatisfying attempt'.)

## Failed attempt 3: Dev branch + dev-pr + squash merge
So I thought of creating a new branch off dev and rebasing that branch instead. I called it dev-pr.

```sh
git fetch origin main:main
git switch --force-create dev-pr dev
git rebase origin/main --autostash
git push --force origin dev-pr
git push origin dev
git switch dev
```

alias:

```sh
git config --global alias.prepr '!git fetch origin main:main && git switch --force-create dev-pr dev && git rebase origin/main --autostash && git push --force origin dev-pr && git push origin dev && git switch dev'
```

so it can simply be run with `git prepr`. Each time you want to make a PR, run this, then create a PR from dev-pr on Github.

But this works only as long as your commits on dev that diverge from main are not touching something that's not the same anymore on main's HEAD (latest commit on main). For example, if you have a commit deleting or changing a file that is no longer there. This is because what's happening under the hood is, each time you run this, it takes main, finds the common ancestor, then applies your subsequent commits in dev one by one on top of it.

Unfortunately this will inevitably eventually break. Just like in attempt 1, you accumulate more and more commits that diverge. Every time you rebase, all these commits have to be applied on top of main's HEAD. If you had a PR that deleted a line and squash-merged it, the next time you run prepr you will have a conflict there, because it tries to apply that commit that is in dev on top.

{% include note.html content="The overall diff is irrelevant. It could be identical and you'd still have conflicts because the way the commits are applied one by one. Git knows how to handle this if it has to apply a commit that has the same patch-id as an already applied commit, but with squash merge we end up with a different patch-id, and it is unable to identify that it has a change that already been applied over n previous separate commits." %}

Simple example to trigger a conflict:

1. make a commit on branch1 that deletes a line (commitA)
2. make a commit on branch1 with some other change (commitB)
3. checkout a new branch called branch2
4. squash the last 2 commits
5. make a commit on branch2 with some other change (commitC)
6. git rebase branch1 branch2

{% include note.html content="If the patch-id is the same though, even if the hash is different (i.e. a rebased/reapplied commit or something, but same diff), it will recognise this is the same as an existing commit and skip it (`warning: skipped previously applied commit <hash>`). So if commitB is an empty commit for example, there will be no conflict." %}

## Imperfect workaround: Rebase onto
Instead of `git rebase branch1 branch2`,

```sh
git rebase --onto branch1 @~2 branch2
```

where 2 is the number of commits to re-apply on top of branch1.

Instead of reapplying all the commits since the common ancestor, it will reapply all the commits since a specified point in history (the @~2 in this case, but it can be any git ref).

## Another imperfect workaround: Cherry pick
Branch dev-pr from main, then cherry-pick from dev only the commits you want for the new PR. You can cherry-pick ranges so it doesn't have to be painstaking.

The problem with both workarounds is it's no longer automatic. There is now introduced a variable. I now have to know which or how many commits I have for the PR. Although in the case of rebase onto it would be fine in most cases to give a number that overshoots the number of real commits that you need in the PR, since most will not have conflicts. So I could put something like @~5 in my alias and continue calling it mindlessly and probably be ok most of the time, until there is a conflict at which point I'd have to modify my alias to how many commits I actually need to apply. This violates requirement 2, and it sounds like pain.

## Feature branches after all?
Maybe you could stop being stubborn and use feature branches like everyone, and reduce the pain by automating it?

One issue with this is don't you have to remember before you make your changes to checkout a new branch? Whereas in the prepr workflow you just make your changes without having to do anything in advance, only calling 1 command when you are ready to make a PR.

And if you don't care about losing history in the branch, as it gets deleted anyway in this workflow, then might as well use [attempt 2](#failed-attempt 2-dev-branch--rebase--squash-merge), which is easier as you never have to switch branches or do anything before you start committing, only run one command when you are ready to make a PR and be done with it.

Also, you will no doubt have noticed a lot of the issues have arisen due to using squash merge. Without squashing at all, it would not meet requirement 3. But a possible alternate workflow that would still meet requirement 3 is to squash manually to the clean history that you want, and then merge in the normal way with a merge commit. But this is extra manual work, violating requirement 2, and would erase your original commits history in any case. Unless you have another branch and keep the original commits in one of the and make the PR from the other, but again we would run into the same issues described in [attempt 3](#failed-attempt-3-dev-branch--dev-pr--squash merge).

## What I'm doing for now
It is far from an ideal solution and more of a stopgap. For now I am sticking to dev and dev-pr with my prepr alias modified to do rebase onto with a specified number of commits to apply.

```sh
git config --global alias.prepr '!git fetch origin main:main && git switch --force-create dev-pr dev && git rebase --onto origin/main @~"${1}" dev-pr --autostash && git push --force origin dev-pr && git push origin dev && git switch dev #'
```

(see [Git alias with positional parameters - Stack Overflow](https://stackoverflow.com/questions/3321492/git-alias-with-positional-parameters). the `#` at the end is important)

With this, if I call just `git prepr`, it applies only the last dev commit on top of main's HEAD. If I call `git prepr 2` it applies the last 2.

Since dev-pr is just re-created from dev each time, inputting the wrong number is not destructive. If you input a larger number than the number of commits you want in your PR, assuming dev is the same as main anyway other than squashed merges, it's not a problem unless one of the commits in the range cannot be applied to main's HEAD, at which point you will get a conflict and have to call `git rebase --abort` and try again. If you input a smaller number than the number of commits you want in your PR, you will not have all the commits in your PR, and if you don't notice this it could be a problem.

I can't think of a non-fragile way to automate figuring out the right number. Possibly all the squash-merged commits commit messages having some indicator that they had been squash-merged, look for the most recent one and only reapply commits after it. But it still seems overcomplicated and fragile.

## Addendum: Why am I thinking about this now
What made me start thinking about this is the book _Software Engineering at Google_, where there were arguments against feature branches, and why they don't use them. That made me raise my eyebrows (both of them). Before this, I don't think I had seen arguments against, and didn't realise there are different workflows, I just thought there is 'proper' and 'improper'.

I have read [Thierry de Pauw's series of articles "On the Evilness of Feature Branching"](https://thinkinglabs.io/articles/2021/04/26/on-the-evilness-of-feature-branching.html), and while he makes some decent arguments ("bring the pain forwards", address conflicts earlier), I think it's overly zealous. There are serious issues and introduced by everyone working in mainline (or even just one person, in my experience), and feature flags are not a magic solution to all. In a real situation there are tradeoffs, every workflow has its downsides, and for each usecase different factors would be more important, leading to the team selecting a different workflow.

I mentioned some of my issues with trunk-based development in the lede. I don't like the fuss over commits while the feature or bugfix is still in development. I think there should be the smallest mental burden to that as possible; the simpler it is, the less stress, and I believe it is important for productivity to accept imperfection and not be under strain to create perfect history.

-----

## Further reading

- [Does squashing pull requests break git's merging algorithm? - Software Engineering Stack Exchange](https://softwareengineering.stackexchange.com/questions/365658/does-squashing-pull-requests-break-gits-merging-algorithm)
- [Git cherry pick and then rebase - Stack Overflow](https://stackoverflow.com/questions/61905448/git-cherry-pick-and-then-rebase)
- [gitworkflows - An overview of recommended workflows with Git](https://git-scm.com/docs/gitworkflows)
- [Software Engineering at Google (2017) - Hacker News](https://news.ycombinator.com/item?id=18818412) (arxiv article, not book)

{% include fin.html %}
