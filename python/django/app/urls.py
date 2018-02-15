from django.urls import path

from . import views

urlpatterns = [
    path(r'', views.index),
    path(r'user/<int:id>', views.get_user),
    path(r'user', views.create_user),
]
