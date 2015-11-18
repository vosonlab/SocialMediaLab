# SocialMediaLab

`SocialMediaLab` is an R package that provides a suite of tools for collecting and constructing networks from social media data. It provides easy-to-use functions for collecting data across popular platforms (Instagram, Facebook, Twitter, and YouTube) and generating different types of networks for analysis.

SocialMediaLab was created by [Tim Graham](http://uq.academia.edu/TimGraham) (who is also the maintainer of the package) and [Robert Ackland](https://researchers.anu.edu.au/researchers/ackland-rj).

The latest 'official' version of the package can also be found on [CRAN](https://cran.r-project.org/web/packages/SocialMediaLab/index.html).

This package would not be possible without key packages by other authors in the R community, notably....

## Getting started

For detailed information and examples, please refer to the [SocialMediaLab documentation](https://github.com/voson-lab/SocialMediaLab/blob/master/SocialMediaLab.pdf).

## Example networks

The following networks were created in SocialMediaLab and visualised using the [Gephi software](http://gephi.github.io/).

##### Facebook bimodal network (Star Wars page)

This network visualises two weeks of activity on the [Star Wars Facebook page](https://www.facebook.com/StarWarsAUNZ/?brand_redir=169299103121699). Nodes (vertices) represent either users or posts, and node ties (edges) represent either 'likes' or comments. Nodes are sized by out-degree and coloured by modularity cluster.

<img src="https://raw.githubusercontent.com/voson-lab/SocialMediaLab/master/miscellaneous/exported_graph_images/Facebook_bimodal_network_socialmedialab_Star_Wars.png?token=AKw5r_WagSdf1f9L-peOJfKZZqIkMYwCks5WVRxNwA%3D%3D" alt="Facebook bimodal network created with SocialMediaLab" width="600" height="600"/>

##### Instagram ego network

This network visualises the social network of one user (the 'ego' node) on Instagram. The degree of the network is "2", meaning that it shows ego + alter ("followers of ego") and ego + alters + alters of alters "followers of followers of ego". 'Follows' data are also collected, so this network also shows "users who ego follows" and "users who follow users who follow ego".

<img src="https://raw.githubusercontent.com/voson-lab/SocialMediaLab/master/miscellaneous/exported_graph_images/Instagram_ego_network_socialmedialab_example.png?token=AKw5r99givdLgt47JH3rMaBA2ZBW9FIoks5WVTJ6wA%3D%3D" alt="Facebook bimodal network created with SocialMediaLab" width="600" height="600"/>

