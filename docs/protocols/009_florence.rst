009 Florence Protocol Change Log
================================

This document contains all the relevant information for protocol 009
Florence. Each of the main changes is briefly described with links to
relevant external documentation and merge requests.

The code for Florence can currently be found in the ``src/proto_alpha``
directory of the ``master`` branch of the `Tezos
repository <https://gitlab.com/tezos/tezos>`__ (release tag and full
hash TBA).

TL;DR
-----

-  Baking Accounts `# <#baking-accounts>`__

   -  New account type enabling key rotation and multisig authentication
      for bakers.

-  Inter-contract calls now execute in depth-first order
   `# <#depth-first-execution-order>`__
-  Migration can now produce balance update receipts
   `# <#migrations-may-now-produce-balance-receipts>`__

   -  Used for migration to baking accounts and developer invoices for
      protocol upgrades.

-  Multiple significant performance improvements `# <#performance>`__

   -  Improved gas accounting
   -  Linear-time endorsement checking
   -  Improved staking balance accounting

-  Operation size limit doubled to 32KB
   `# <#increase-max_operation_data_length-to-32kb>`__
-  Test chain removed from the voting procedure
   `# <#deactivation-of-the-test-chain-in-the-economic-protocol>`__
-  A new version of the environment

   -  See Environment change log for details (TODO: create said document
      per `issue <https://gitlab.com/tezos/tezos/-/issues/1083>`__)

All MR’s (XX in total)
----------------------

TODO: update final MR count

Baking Accounts
~~~~~~~~~~~~~~~

A new account type has been added to represent accounts managed
by bakers. These accounts are Michelson smart contracts running a fixed
multisig script. This feature lets bakers renew and split their
consensus keys without moving to a new address and asking their
delegators to follow. In addition to the usual internal operations,
baking accounts can also emit baking operations such as proposing and
voting for protocol amendments.

-  TZIP:
   `tzip!133 <https://gitlab.com/tzip/tzip/-/merge_requests/133>`__
-  MR:
   `tezos!2458 <https://gitlab.com/tezos/tezos/-/merge_requests/2458>`__
-  Docs:
   `tezos!2490 <https://gitlab.com/tezos/tezos/-/merge_requests/2490>`__

Smart Contracts/Michelson
~~~~~~~~~~~~~~~~~~~~~~~~~

Increase ``max_operation_data_length`` to 32KB
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The maximal size of operations is doubled. Among other things, this
permits creation of larger smart contracts.

-  Commit:
   `tezos#3ff6bc8d <https://gitlab.com/tezos/tezos/commit/3ff6bc8da9f8941b65fb9be4e51d3de1e93bfaed>`__
-  TZIP:
   `increase_operation_size_limit <https://gitlab.com/tzip/tzip/-/blob/master/drafts/current/draft-increase_operation_size_limit.md>`__

Fixed a discrepancy between ``CONTRACT`` and ``PACK`` in addresses without entrypoints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  Fixes issue:
   `tezos#643 <https://gitlab.com/tezos/tezos/-/issues/643>`__
-  Commit:
   `tezos#e879b1a7 <https://gitlab.com/tezos/tezos/commit/e879b1a764ed95182ce33b0a13e0f807f21520ed>`__

Depth-First Execution Order
^^^^^^^^^^^^^^^^^^^^^^^^^^^

The applied order of inter-contract calls emitted by smart contracts has
changed. The operations are now placed in a stack instead of a queue,
resulting in a depth-first as opposed to breadth-first execution order.

-  TZIP:
   `tzip!111 <https://gitlab.com/tzip/tzip/-/merge_requests/111>`__
-  MR:
   `tezos!2420 <https://gitlab.com/tezos/tezos/-/merge_requests/2420>`__

Tooling
~~~~~~~

Normalization RPCs
^^^^^^^^^^^^^^^^^^

Two new normalization RPCs, ``normalize_data`` and ``normalize_script``,
have been added. They can be used to convert Michelson values and
scripts that have multiple possible representations into a canonical
format. In particular, these RPCs can be used to convert Michelson comb
pairs into the format they had before the introduction of the compact
notations in Edo.

-  Addresses issue:
   `tezos#1016 <https://gitlab.com/tezos/tezos/-/issues/1016>`__
-  MR:
   `tezos!2354 <https://gitlab.com/tezos/tezos/-/merge_requests/2354>`__

New ``failing_noop`` Operation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A new operation has been added to the protocol that is guaranteed to
fail. This feature can be used by tooling (such as ``tezos-client``) to
sign arbitrary data securely, without fear of malicious injection into
future protocols. 

- Adresses issue:
  `tezos#52 <https://gitlab.com/metastatedev/tezos/-/issues/52>`__ 
- MR:
  `tezos!2361 <https://gitlab.com/tezos/tezos/-/merge_requests/2361>`__

Performance
~~~~~~~~~~~

Endorsements Now Checked in Linear Time
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-  Closes issue:
   `tezos#1028 <https://gitlab.com/tezos/tezos/-/issues/1028>`__
-  MR:
   `tezos!2471 <https://gitlab.com/tezos/tezos/-/merge_requests/2471>`__

Staking balance RPC
^^^^^^^^^^^^^^^^^^^

Some users observed degraded performance in v8.1 as reported in issue
`tezos#1067 <https://gitlab.com/tezos/tezos/-/issues/1067>`__. To
address this, the measurement of staking balance has been reworked,
improving the performance of the
``/chains/[...]/blocks/[...]/context/delegates/[...]`` RPC endpoint.

-  MR:
   `tezos!2547 <https://gitlab.com/tezos/tezos/-/merge_requests/2547>`__

Gas Optimizations
^^^^^^^^^^^^^^^^^

Various optimizations have been added to the gas accounting subsystem.
Most notably, gas consumption is now computed using `saturated
arithmetic <https://en.wikipedia.org/wiki/Saturation_arithmetic>`__.

-  MR’s:
   `tezos!2328 <https://gitlab.com/tezos/tezos/-/merge_requests/2328>`__,
   `tezos!2327 <https://gitlab.com/tezos/tezos/-/merge_requests/2327>`__,
   and
   `tezos!2329 <https://gitlab.com/tezos/tezos/-/merge_requests/2329>`__

Governance
~~~~~~~~~~

Deactivation of the Test Chain in the Economic Protocol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Prior to Florence, Tezos nodes spawned a test chain during the “Testing”
phase of voting for the purpose of allowing users to test the new
amendment. However, this feature was both unused in practice and quite
complex. It has been removed, simplifying the amendment protocol.

-  TZIP:
   `tzip!141 <https://gitlab.com/tzip/tzip/-/merge_requests/141>`__
-  MR:
   `tezos!2469 <https://gitlab.com/tezos/tezos/-/merge_requests/2469>`__

Migration
~~~~~~~~~

Migrations may now Produce Balance Receipts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Protocol migrations can now update the balance of accounts by producing
balance receipts. This was necessary groundwork for `Baking
Accounts <#baking-accounts>`__ and facilitates `developer
invoicing <https://www.youtube.com/watch?v=VFY76qFq5Gk>`__.

-  Issue:
   `tezos#138 <https://gitlab.com/metastatedev/tezos/-/issues/138>`__
-  MR:
   `tezos!2437 <https://gitlab.com/tezos/tezos/-/merge_requests/2437>`__

Internal
~~~~~~~~

Refactoring
^^^^^^^^^^^

Abstract protocol types can now be used consistently outside the
protocol.

-  MR:
   `tezos!2497 <https://gitlab.com/tezos/tezos/-/merge_requests/2497>`__
