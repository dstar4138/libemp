# LibEMP Event Sinks

LibEMP Event Sinks encapsulate the processing of events. They are potentially
stackable which means they are capable of being used as a work-flow triggered
by the events sent to a Buffer. They need to be wired up to initialize this
flow upfront into a Processor Stack which is in theory stateless enough to be
replicated and load balanced across local cores at the very least.

Each of the subdirectories contained herein implement example and useful Sinks
which can be used in your own Work-Flows. Since Sinks contain the majority of
your business logic we leave their implementation up to you and provide an
extremely simple API and user interface.

