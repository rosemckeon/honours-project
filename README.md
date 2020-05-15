# When rare gametes meet

This repository is my undergraduate honours project, and includes:

- An individual-based model, written as an R package *sploidy*, that simulates the evolution of polyploidy in flowering plants. 
- The [scripts](https://github.com/rosemckeon/honours-project/blob/master/scripts/) and [data](https://github.com/rosemckeon/honours-project/blob/master/data/) output by running the model, parametised to simulate the life-cycle of *Erythranthe guttata* (formerly: *Mimulus guttatus*).
- The [thesis itself](https://github.com/rosemckeon/honours-project/blob/master/thesis/_thesis_2417024.pdf) which explains the model, and all the [files](https://github.com/rosemckeon/honours-project/blob/master/thesis/) that went into creating that final document.
- A tagged version of the model in the state in which it was used to inform my thesis (v0.2.2).

---

Genome-doubling readily occurs in flowering plants (angiosperms) and is linked with speciation as well as major innovations (seeds and flowers). Beneficial traits (increased flower size and number) are often expressed by the resulting polyploid species, making them useful crops as well as increasing their invasive potential. However, after a century of research, the forces that drive polyploid evolution are still unclear. The primary mechanism of formation by gametic nonreduction creates an inherent negative selection pressure by the interaction of triploid sterility and pollen-swamping. I ask whether the combined cost of these emergent properties can be quantified by using an individual-based modelling approach. Specifically, I investigate whether increasing the rate of nonreduction is sufficient to give evolving polyploids enough of an advantage that they can reach stable fixation. Modelling this system revealed that in order to ultimately outcompete their diploid progenitors (as well as the sterile offspring that interploidy matings create), the rate of nonreduction had to exceed the natural rate 27-fold. Extinction probability above this rate was 0.686, and fixation of polyploidy was extremely unstable. The rate of nonreduction required to achieve stable fixation was 83-fold greater than the natural rate. Extinction probability reached 0.719, and the probability of attaining stable fixation was 0.070. These findings support the idea that polyploidy most often leads to evolutionary 'dead-ends'. They also indicate that the strength of triploid sterility must be drastically reduced, or some other beneficial adaptation would be required in order for polyploidy to evolve and persist.

## Installation of the R package

In an R console:
```
install.packages("devtools")
devtools::install_github("rosemckeon/honours-project")
library(sploidy)
?sploidy
```
