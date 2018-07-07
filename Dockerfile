FROM python
LABEL maintainer="Tycho Grouwstra <tychogrouwstra@gmail.com>"
# RUN apt-get update && apt-get install -y python-opengl
# ENV DISPLAY unix$DISPLAY
# VOLUME ["/tmp/.X11-unix"]
RUN curl https://bootstrap.pypa.io/get-pip.py | python -
COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt
COPY gym_http_server.py gym_http_server.py
EXPOSE 5000
CMD ["python", "gym_http_server.py", "-l", "0.0.0.0"]

