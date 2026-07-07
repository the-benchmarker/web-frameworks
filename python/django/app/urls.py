"""
Django Benchmark URLs

URL configuration for benchmark endpoints.
Follows Django best practices for URL routing.
"""

from django.urls import path

from . import views

app_name = "benchmark"

urlpatterns = [
    # Benchmark endpoints
    path(route="", view=views.index, name="index"),
    path(route="user/<int:id>", view=views.get_user, name="get_user"),
    path(route="user", view=views.create_user, name="create_user"),
    # Health check endpoint
    path(route="health", view=views.health_check, name="health_check"),
]
